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
        ( ClientConfig
        , ClientType(..)
        , Technology(..)
        , clientTypeFromString
        , clientTypeToString
        , getConfig
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
import Page.Components.Header as Header
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
    , clientConfig : ConfigStatus ClientConfig

    -- Loaded independently from server
    }


type Problem
    = ServerError String


type CallState
    = Initial
    | CallStarted
    | CallStopped
    | Loading
    | Failed


type ConfigStatus a
    = ConfigInitial
    | ConfigSuccess a
    | ConfigFailed


init : Session -> Maybe String -> ( Model, Cmd Msg )
init session queryParam =
    let
        configCmd =
            Http.send RequestConfig getConfig
    in
    ( { title = "Initial state"
      , session = session
      , problems = []
      , state = Initial
      , timeZone = Time.utc
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
              }
            , Cmd.none
            )

        ChangeState state ->
            ( { model | state = state }, Cmd.none )



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
        ( clientType, firstTechnology ) =
            case model.clientConfig of
                ConfigSuccess a ->
                    ( a.type_, Maybe.withDefault VNC (List.head a.technologies) )

                _ ->
                    ( Guest, VNC )
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
                Initial ->
                    ( button [ css [ Styles.startButton ] ] [ text "Start Call" ], ChangeState CallStarted )

                CallStarted ->
                    ( button [ css [ Styles.stopButton ] ] [ text "Stop Call" ], ChangeState CallStopped )

                CallStopped ->
                    ( button [ css [ Styles.startButton ] ] [ text "Start Call" ], ChangeState CallStarted )

                Loading ->
                    ( button [ css [ Styles.stopButton ] ] [ text "Stop Call" ], ChangeState CallStarted )

                Failed ->
                    ( button [ css [ Styles.stopButton ] ] [ text "Stop Call" ], ChangeState CallStarted )
    in
    if clientType == Presenter then
        div [ onClick action ] [ btn ]

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


technologyToString : Technology -> String
technologyToString tech =
    case tech of
        VNC ->
            "VNC"

        WebRTC ->
            "WebRTC"


stateToString : CallState -> String
stateToString state =
    case state of
        Initial ->
            "Initial"

        Loading ->
            "Loading"

        Failed ->
            "Failed"

        CallStarted ->
            "Started"

        CallStopped ->
            "Stopped"
