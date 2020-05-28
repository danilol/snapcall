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
import Utils.Html exposing (select)



-- MODEL


type alias Model =
    { title : String
    , session : Session
    , problems : List Problem
    , timeZone : Time.Zone
    , state : CallState
    , client : ClientConfig

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


init : Session -> Maybe String -> ( Model, Cmd Msg )
init session queryParam =
    let
        clienttTypeStr =
            clientTypeFromString queryParam

        clientConfig =
            Client.init clienttTypeStr
    in
    ( { title = "Initial state"
      , session = session
      , problems = []
      , state = Initial
      , timeZone = Time.utc
      , client = clientConfig
      }
    , Task.perform GotTimeZone Time.here
    )



-- UPDATE


type Msg
    = GotTimeZone Time.Zone
    | ChangeState CallState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        ChangeState state ->
            ( { model | state = state }, Cmd.none )



--SetClient client ->
--( { model | client = Just client }, Cmd.none )
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
        , client : ClientConfig
    }
    -> { title : String, content : Html Msg }
view model =
    { title = model.title
    , content =
        case model.state of
            Initial ->
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
                                                ++ (clientTypeToString <| Just model.client.type_)
                                                ++ " "
                                                ++ (technologyToString <| Just model.client.technology)
                                            )
                                        ]
                                    ]
                                , if model.client.type_ == Presenter then
                                    div [ onClick <| ChangeState CallStarted ]
                                        [ button [ css [ Styles.startButton ] ] [ text "Start Call" ]

                                        --, button [ onClick <| SetClient Guest ] [ text "Join as a guest" ]
                                        ]

                                  else
                                    div [] []
                                ]
                            ]
                        ]
                    ]

            Loading ->
                div [ css [ resetStyles ] ]
                    [ Header.view
                    , div
                        [ css [ container, marginTop (px 50), width (pct 50) ] ]
                        [ div [ css [ shadow ] ]
                            [ div [ css [ Styles.dataHeader ] ] [ h1 [ css [ margin zero ] ] [ text "Snapview" ] ]

                            --, video [ id "videoTag" ] []
                            , Styled.table [ css [ Styles.table, tableStriped ] ]
                                [ tbody []
                                    [ tr []
                                        [ td []
                                            [ text "Picture" ]
                                        , td []
                                            [ img
                                                [ Asset.src Asset.srCall
                                                , css [ width (px 300) ]
                                                ]
                                                []
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]

            Failed ->
                div
                    [ css [ resetStyles ] ]
                    [ Header.view
                    , viewProblems model
                    ]

            CallStarted ->
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
                                                ++ (clientTypeToString <| Just model.client.type_)
                                                ++ " "
                                                ++ (technologyToString <| Just model.client.technology)
                                            )
                                        ]
                                    ]
                                , if model.client.type_ == Presenter then
                                    div [ onClick <| ChangeState CallStopped ]
                                        [ button [ css [ Styles.stopButton ] ] [ text "Stop Call" ]

                                        --, button [ onClick <| SetClient Guest ] [ text "Join as a guest" ]
                                        ]

                                  else
                                    div [] []
                                ]
                            ]
                        ]
                    ]

            CallStopped ->
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
                                                ++ (clientTypeToString <| Just model.client.type_)
                                                ++ " "
                                                ++ (technologyToString <| Just model.client.technology)
                                            )
                                        ]
                                    ]
                                , if model.client.type_ == Presenter then
                                    div [ onClick <| ChangeState CallStarted ]
                                        [ button [ css [ Styles.startButton ] ] [ text "Start Call" ]

                                        --, button [ onClick <| SetClient Guest ] [ text "Join as a guest" ]
                                        ]

                                  else
                                    div [] []
                                ]
                            ]
                        ]
                    ]
    }



-- SEARCH view


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


technologyToString : Maybe Technology -> String
technologyToString tech =
    case tech of
        Nothing ->
            ""

        Just t ->
            case t of
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
