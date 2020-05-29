module Page.Call exposing
    ( Model
    , Msg
    , Problem(..)
    , init
    , toSession
    , update
    , view
    )

{-| The call page. You can get here via the /call route.
-}

import Api exposing (decodeError)
import Asset
import Client
    exposing
        ( AcknowledgmentMetadata
        , CallState(..)
        , ClientConfig
        , ClientType(..)
        , SharingOption(..)
        , Technology(..)
        , acknowledgeRequest
        , clientTypeFromString
        , clientTypeToString
        , getConfig
        , sharingOptionToString
        , stateToString
        , technologyToString
        )
import Css
    exposing
        ( color
        , fontWeight
        , hex
        , int
        , listStyle
        , margin
        , marginBottom
        , marginTop
        , none
        , padding
        , pct
        , px
        , width
        , zero
        )
import Html.Styled as Styled
    exposing
        ( Html
        , button
        , div
        , h1
        , h2
        , img
        , li
        , p
        , span
        , tbody
        , td
        , text
        , tr
        , ul
        , video
        )
import Html.Styled.Attributes exposing (css, id)
import Html.Styled.Events exposing (onClick)
import Http
import Page.Components.Button as Button exposing (Button, State(..))
import Page.Components.Dialog as Dialog
import Page.Components.Header as Header
import Process
import Session exposing (Session)
import Styles
    exposing
        ( container
        , resetStyles
        , shadow
        , sharingOptionContainer
        , sharingOptionContent
        , tableStriped
        , textCenter
        )
import Task
import Time
import Utils.Html exposing (emptyHtml, select)



-- MODEL


type alias Model =
    { title : String
    , session : Session
    , problems : List Problem
    , timeZone : Time.Zone
    , state : CallState
    , mobile : Bool
    , clientConfig : ConfigStatus ClientConfig
    , showScreenDialog : Bool
    , sharingOption : Maybe SharingOption

    --, activeTab : Maybe String
    }


type Problem
    = ServerError String


type ConfigStatus a
    = ConfigInitial
    | ConfigSuccess a
    | ConfigFailed


{-| Scenarios :
Scenario 1:
presenter
mobileFlag = false -> VNC
/call

Scenario 2:
guest
mobileFlag = true -> VNC
/call?userType=guest

Scenario 3:
presenter
mobileFlag = false -> WebRTC
/call?technology=WebRTC

Scenario 4:
guest
mobileFlag = false -> WebRTC
/call?technology=WebRTC

Scenario 5:
presenter
mobileFlag = false -> VNC
/call?technology=WebRTC
ERROR

-}
init : Session -> Maybe String -> Maybe String -> ( Model, Cmd Msg )
init session guestParam technologyParam =
    let
        mobile =
            case guestParam of
                --  if has param => Guest else => Presenter
                Nothing ->
                    False

                Just _ ->
                    True

        rtc =
            case technologyParam of
                --  if has param => WebRTC else => VNC
                Nothing ->
                    False

                Just _ ->
                    True

        ( state, sharingOption ) =
            if mobile then
                ( CallStarted, Just Mobile )

            else
                ( Initial, Nothing )
    in
    ( { title = "Snapview call"
      , session = session
      , problems = []
      , state = state
      , timeZone = Time.utc
      , mobile = mobile
      , clientConfig = ConfigInitial
      , sharingOption = sharingOption

      -- screen control
      , showScreenDialog = False
      }
    , Cmd.batch
        [ Task.perform GotTimeZone Time.here
        , Http.send RequestConfig <| getConfig { mobileFlag = mobile, rtcPriority = rtc }
        ]
    )



-- UPDATE


type Msg
    = GotTimeZone Time.Zone
    | RequestConfig (Result Http.Error ClientConfig)
    | ChangeState CallState
    | DelayAndChangeState CallState
    | Acknowledgment (Result Http.Error AcknowledgmentMetadata)
    | OpenScreenShareModal
    | CloseScreenShareModal
    | SetSharingOption SharingOption
    | StopScreenSharing
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        RequestConfig (Ok config) ->
            ( { model | clientConfig = ConfigSuccess config }, Cmd.none )

        RequestConfig (Err error) ->
            let
                serverErrors =
                    [ ServerError (decodeError error) ]
            in
            ( { model
                | clientConfig = ConfigFailed
                , problems = serverErrors
                , state = Failed
              }
            , Cmd.none
            )

        DelayAndChangeState state ->
            ( { model | state = Client.Loading }
              -- to give the load effect
            , Process.sleep 2000
                |> Task.perform (always (ChangeState state))
            )

        ChangeState state ->
            let
                cmd =
                    Http.send Acknowledgment <|
                        acknowledgeRequest
                            { technology = extractTechnology model.clientConfig
                            , mobileFlag = False
                            , state = state
                            , sharingOption = model.sharingOption
                            }
            in
            ( { model | state = state }, cmd )

        Acknowledgment (Ok acknowledge) ->
            let
                st =
                    case acknowledge.state of
                        StartAttempt ->
                            CallStarted

                        _ ->
                            acknowledge.state
            in
            ( { model | state = st }, Cmd.none )

        Acknowledgment (Err error) ->
            let
                serverErrors =
                    [ ServerError (decodeError error) ]
            in
            ( { model
                | clientConfig = ConfigFailed
                , state = Failed
                , problems = serverErrors
              }
            , Cmd.none
            )

        OpenScreenShareModal ->
            ( { model | showScreenDialog = True }, Cmd.none )

        CloseScreenShareModal ->
            ( { model | showScreenDialog = False }, Cmd.none )

        SetSharingOption opt ->
            let
                newSharingOption =
                    Just opt
            in
            ( { model
                | sharingOption = newSharingOption
                , showScreenDialog = False
              }
            , Http.send Acknowledgment <|
                acknowledgeRequest
                    { technology = extractTechnology model.clientConfig
                    , mobileFlag = model.mobile
                    , state = model.state
                    , sharingOption = newSharingOption
                    }
            )

        StopScreenSharing ->
            ( { model | sharingOption = Nothing }
            , Http.send Acknowledgment <|
                acknowledgeRequest
                    { technology = extractTechnology model.clientConfig
                    , mobileFlag = model.mobile
                    , state = model.state
                    , sharingOption = Nothing
                    }
            )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS
-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- HTTP
-- VIEW


view :
    { model
        | title : String
        , problems : List Problem
        , timeZone : Time.Zone
        , state : CallState
        , clientConfig : ConfigStatus ClientConfig
        , showScreenDialog : Bool
        , sharingOption : Maybe SharingOption
    }
    -> { title : String, content : Html Msg }
view model =
    let
        clientType =
            extractClientType model.clientConfig

        firstTechnology =
            extractTechnology model.clientConfig

        isSharing =
            case model.sharingOption of
                Just _ ->
                    True

                Nothing ->
                    False

        stateText =
            case model.state of
                Initial ->
                    "Click to start the call"

                Client.Loading ->
                    "Loading..."

                Client.CallPaused ->
                    "Transmition Paused..."

                Client.CallFinished ->
                    "Click to start a new call"

                _ ->
                    if isSharing then
                        "You are " ++ sharingOptionToString model.sharingOption

                    else
                        "Your are sharing your camera and it's off..."
    in
    { title = model.title
    , content =
        div [ css [ resetStyles ] ]
            [ Header.view
            , div
                [ css [ container, marginTop (px 50) ] ]
                [ div [ css [ shadow ] ]
                    [ div
                        [ css [ Styles.dataHeader ]
                        ]
                        [ h1 [ css [ margin zero ] ] [ text <| "Snapview: " ++ stateToString model.state ] ]
                    , div [ css [ Styles.cameraContainter ] ]
                        [ div [ css [ Styles.camera ] ]
                            [ h1 [ css [ margin zero ] ] [ text stateText ]
                            , div [ css [ margin zero ] ] [ text ("ClientType: " ++ clientTypeToString clientType) ]
                            , div [ css [ margin zero ] ] [ text ("Tech Type: " ++ technologyToString firstTechnology) ]
                            , if isSharing then
                                div [ css [ margin zero ] ] [ text ("Selection Name: " ++ sharingOptionToString model.sharingOption) ]

                              else
                                emptyHtml
                            ]
                        , callFlowButtonView model clientType
                        , shareScreenButtonView model clientType
                        , pauseButtonView model
                        , Dialog.view
                            { visible = model.showScreenDialog
                            , cancelAction = CloseScreenShareModal
                            , message = Styled.toUnstyled <| screenSharingModalView firstTechnology
                            }
                        ]
                    ]
                ]
            ]
    }


callFlowButtonView : { model | state : CallState } -> ClientType -> Html Msg
callFlowButtonView model clientType =
    let
        ( btn, action ) =
            case model.state of
                Client.Initial ->
                    --Button.view (Button "Start Call" Valid "") (DelayAndChangeState StartAttempt)
                    ( button [ css [ Styles.greenButton ] ] [ text "Start Call" ], DelayAndChangeState StartAttempt )

                Client.StartAttempt ->
                    ( button [ css [ Styles.greenButton ] ] [ text "..." ], ChangeState StartAttempt )

                Client.CallStarted ->
                    ( button [ css [ Styles.redButton ] ] [ text "Finish Call" ], ChangeState CallFinished )

                Client.CallPaused ->
                    ( button [ css [ Styles.redButton ] ] [ text "Finish Call" ], ChangeState CallFinished )

                Client.CallUnpaused ->
                    ( button [ css [ Styles.redButton ] ] [ text "Finish Call" ], ChangeState CallFinished )

                Client.CallFinished ->
                    ( button [ css [ Styles.greenButton ] ] [ text "Start Call" ], DelayAndChangeState StartAttempt )

                Client.Loading ->
                    ( button [ css [ Styles.greenButton ] ] [ text "..." ], ChangeState CallStarted )

                _ ->
                    ( button [ css [ Styles.redButton ] ] [ text "Stop Call" ], ChangeState CallStarted )
    in
    if clientType == Presenter then
        div [ onClick action ] [ btn ]
        --btn

    else
        emptyHtml


pauseButtonView :
    { model
        | state : CallState
        , sharingOption : Maybe SharingOption
    }
    -> Html Msg
pauseButtonView model =
    case model.state of
        Client.CallStarted ->
            div [ onClick <| ChangeState CallPaused ] [ button [ css [ Styles.greyButton ] ] [ text "Pause Call" ] ]

        Client.CallPaused ->
            div [ onClick <| ChangeState CallUnpaused ] [ button [ css [ Styles.greyButton ] ] [ text "Resume Call" ] ]

        Client.CallUnpaused ->
            div [ onClick <| ChangeState CallPaused ] [ button [ css [ Styles.greyButton ] ] [ text "Pause Call" ] ]

        _ ->
            emptyHtml


shareScreenButtonView :
    { model
        | state : CallState
        , sharingOption : Maybe SharingOption
    }
    -> ClientType
    -> Html Msg
shareScreenButtonView model clientType =
    let
        content =
            case model.sharingOption of
                Nothing ->
                    case model.state of
                        Client.CallStarted ->
                            div []
                                [ button [ css [ Styles.blueButton ], onClick OpenScreenShareModal ]
                                    [ text "Present now" ]
                                ]

                        Client.CallUnpaused ->
                            div []
                                [ button [ css [ Styles.blueButton ], onClick OpenScreenShareModal ]
                                    [ text "Present now" ]
                                ]

                        _ ->
                            emptyHtml

                Just _ ->
                    case model.state of
                        Client.CallStarted ->
                            div []
                                [ button [ css [ Styles.blueButton ], onClick StopScreenSharing ]
                                    [ text "Stop Presenting" ]
                                ]

                        Client.CallUnpaused ->
                            div []
                                [ button [ css [ Styles.blueButton ], onClick StopScreenSharing ]
                                    [ text "Stop Presenting" ]
                                ]

                        _ ->
                            emptyHtml
    in
    if clientType == Presenter then
        content

    else
        emptyHtml


screenSharingModalView : Technology -> Html Msg
screenSharingModalView technology =
    case technology of
        VNC ->
            div []
                [ span [] [ text "You are about to start Screen sharing:" ]
                , div [ css [ sharingOptionContainer ] ]
                    [ div [ css [ sharingOptionContent ], onClick <| SetSharingOption Screen ] [ text "Select Screen" ]
                    , div [ css [ sharingOptionContent ], onClick <| SetSharingOption Window ] [ text "Select Window" ]
                    ]
                ]

        WebRTC ->
            div []
                [ span [] [ text "You are about to start Screen sharing:" ]
                , div [ css [ sharingOptionContainer ] ]
                    [ div [ css [ sharingOptionContent ], onClick <| SetSharingOption Screen ] [ text "Select Screen" ]
                    ]
                ]



-- ERRORS view


viewProblems : { model | problems : List Problem } -> Html msg
viewProblems model =
    div
        [ css
            [ container
            , textCenter
            , marginTop (px 50)
            ]
        ]
        [ ul
            [ css
                [ margin zero
                , padding zero
                , listStyle none
                ]
            ]
            (List.map viewProblem model.problems)
        , img [ Asset.src Asset.error, css [ width (px 300) ] ] []
        ]


viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                ServerError str ->
                    str
    in
    li []
        [ h2
            [ css
                [ margin zero
                , marginBottom (px 60)
                , color (hex "3398cc")
                , fontWeight (int 400)
                ]
            ]
            [ text errorMessage
            ]
        ]



-- HELPERS


extractTechnology : ConfigStatus ClientConfig -> Technology
extractTechnology config =
    case config of
        ConfigSuccess a ->
            Maybe.withDefault VNC (List.head a.technologies)

        _ ->
            VNC


extractClientType : ConfigStatus ClientConfig -> ClientType
extractClientType config =
    case config of
        ConfigSuccess a ->
            a.type_

        _ ->
            Guest
