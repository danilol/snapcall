module Utils.Chrono exposing
    ( Model
    , Msg
    , init
    , start
    , stop
    , subscriptions
    , tick
    , update
    , view
    )

{-| The module to control time. We get the current time, time the meeting started and the duration.
-}

import Html.Styled exposing (Html, h1, text)
import Task
import Time exposing (millisToPosix, posixToMillis)
import Utils.Html exposing (emptyHtml)



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Maybe Time.Posix
    , timeStarted : Maybe Time.Posix
    , duration : Maybe Time.Posix
    }


init : ( Model, Cmd Msg )
init =
    ( { zone = Time.utc
      , time = Just (Time.millisToPosix 0)
      , timeStarted = Nothing
      , duration = Nothing
      }
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Start
    | Stop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = Just newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        Start ->
            ( { model | timeStarted = model.time }
            , Cmd.none
            )

        Stop ->
            ( { model | timeStarted = Nothing }
            , Cmd.none
            )


tick : Model -> Sub Msg
tick _ =
    Time.every 1000 Tick


start : Model -> ( Model, Cmd Msg )
start model =
    update Start model


stop : Model -> ( Model, Cmd Msg )
stop model =
    update Stop model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        duration =
            case ( model.timeStarted, model.time ) of
                -- we only have duration if we have both started and current time
                ( Just started, Just now ) ->
                    let
                        result =
                            posixToMillis now - posixToMillis started
                    in
                    toTimeDisplay (millisToPosix result) model.zone

                _ ->
                    emptyHtml
    in
    duration


toTimeDisplay : Time.Posix -> Time.Zone -> Html Msg
toTimeDisplay time zone =
    let
        hour =
            -- - 2 is the workaround to handle the timezone XD
            String.pad 2 '0' (String.fromInt <| Time.toHour zone time - 2)

        minute =
            String.pad 2 '0' (String.fromInt <| Time.toMinute zone time)

        second =
            String.pad 2 '0' (String.fromInt <| Time.toSecond zone time)
    in
    h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
