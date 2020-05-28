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
        , Technology(..)
        , acknowledgeRequest
        , clientTypeFromString
        , clientTypeToString
        , getConfig
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
import Page.Components.Header as Header
import Process
import Session exposing (Session)
import Styles
    exposing
        ( container
        , resetStyles
        , shadow
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

    -- Loaded independently from server
    }


type Problem
    = ServerError String


type ConfigStatus a
    = ConfigInitial
    | ConfigSuccess a
    | ConfigFailed


init : Session -> Maybe String -> ( Model, Cmd Msg )
init session queryParam =
    let
        configCmd =
            case queryParam of
                -- with param == Guest, without == Presenter
                Nothing ->
                    Http.send RequestConfig <| getConfig True

                Just q ->
                    Http.send RequestConfig <| getConfig False
    in
    ( { title = "Snapview call"
      , session = session
      , problems = []
      , state = Initial
      , timeZone = Time.utc
      , mobile = False
      , clientConfig = ConfigInitial
      }
    , Cmd.batch
        [ Task.perform GotTimeZone Time.here
        , configCmd
        ]
    )



-- UPDATE


type Msg
    = GotTimeZone Time.Zone
    | RequestConfig (Result Http.Error ClientConfig)
    | ChangeState CallState
    | DelayAndChangeState CallState
    | Acknowledgment (Result Http.Error AcknowledgmentMetadata)


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
            , Process.sleep 2000 |> Task.perform (always (ChangeState state))
            )

        ChangeState state ->
            let
                cmd =
                    Http.send Acknowledgment <|
                        acknowledgeRequest
                            { technology = extractTechnology model.clientConfig
                            , mobileFlag = False
                            , state = state
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
                _ =
                    Debug.log "errror" error

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
    }
    -> { title : String, content : Html Msg }
view model =
    let
        clientType =
            extractClientType model.clientConfig

        firstTechnology =
            extractTechnology model.clientConfig
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
                            [ h1 [ css [ margin zero ] ]
                                [ text
                                    ("Camera is off : "
                                        ++ clientTypeToString clientType
                                        ++ " "
                                        ++ technologyToString firstTechnology
                                    )
                                ]
                            ]
                        , callBtnPartial model clientType
                        ]
                    ]
                ]
            ]
    }


callBtnPartial : { model | state : CallState } -> ClientType -> Html Msg
callBtnPartial model clientType =
    let
        ( btn, action ) =
            case model.state of
                Client.Initial ->
                    --Button.view (Button "Start Call" Valid "") (DelayAndChangeState StartAttempt)
                    ( button [ css [ Styles.startButton ] ] [ text "Start Call" ], DelayAndChangeState StartAttempt )

                Client.StartAttempt ->
                    --Button.view (Button "..." Valid "") (DelayAndChangeState StartAttempt)
                    ( button [ css [ Styles.startButton ] ] [ text "..." ], ChangeState StartAttempt )

                Client.CallStarted ->
                    ( button [ css [ Styles.stopButton ] ] [ text "Stop Call" ], ChangeState CallStopped )

                Client.CallStopped ->
                    ( button [ css [ Styles.startButton ] ] [ text "Start Call" ], DelayAndChangeState StartAttempt )

                Client.Loading ->
                    ( button [ css [ Styles.startButton ] ] [ text "..." ], ChangeState CallStarted )

                _ ->
                    --Button.view (Button "test" Button.Loading "") (DelayAndChangeState StartAttempt)
                    ( button [ css [ Styles.stopButton ] ] [ text "Stop Call" ], ChangeState CallStarted )
    in
    if clientType == Presenter then
        div [ onClick action ] [ btn ]
        --btn

    else
        emptyHtml



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
