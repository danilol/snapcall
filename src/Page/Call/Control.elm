module Page.Call.Control exposing
    ( finishButton
    , initialFinishButton
    , initialPauseButton
    , initialPresentButton
    , initialStartButton
    , pauseButton
    , presentButton
    , startButton
    )

import Client exposing (CallState(..))
import Html.Styled exposing (Html)
import Page.Components.Button as Button exposing (Config, State(..))
import Utils.Html exposing (emptyHtml)



-- initial Button states


initialStartButton : msg -> Config msg
initialStartButton msg =
    { label = "Start"
    , look = Button.Green
    , state = Button.Loading
    , size = Button.Small
    , onClick = msg
    , extraCss = []
    }


initialPauseButton : msg -> Config msg
initialPauseButton msg =
    { label = "Pause"
    , look = Button.Grey
    , state = Button.Enabled
    , size = Button.Small
    , onClick = msg
    , extraCss = []
    }


initialFinishButton : msg -> Config msg
initialFinishButton msg =
    { label = "Finish"
    , look = Button.Red
    , state = Button.Enabled
    , size = Button.Small
    , onClick = msg
    , extraCss = []
    }


initialPresentButton : msg -> Config msg
initialPresentButton msg =
    { label = "Present now"
    , look = Button.Violet
    , state = Button.Enabled
    , size = Button.Small
    , onClick = msg
    , extraCss = []
    }



-- Button controls - when to display or hide


startButton : { model | startButton : Button.Config msg, state : CallState } -> Html msg
startButton model =
    case model.state of
        LoadingConfig ->
            Button.view model.startButton

        ConfigLoaded ->
            Button.view model.startButton

        StartAttempt ->
            Button.view model.startButton

        CallFinished ->
            Button.view model.startButton

        _ ->
            emptyHtml


pauseButton : { model | pauseButton : Button.Config msg, state : CallState } -> Html msg
pauseButton model =
    case model.state of
        CallStarted ->
            Button.view model.pauseButton

        CallPaused ->
            Button.view model.pauseButton

        PauseAttempt ->
            Button.view model.pauseButton

        CallUnpaused ->
            Button.view model.pauseButton

        UnpauseAttempt ->
            Button.view model.pauseButton

        PresentStarted ->
            Button.view model.pauseButton

        PresentStopped ->
            Button.view model.pauseButton

        _ ->
            emptyHtml


finishButton : { model | finishButton : Button.Config msg, state : CallState } -> Html msg
finishButton model =
    case model.state of
        CallStarted ->
            Button.view model.finishButton

        CallPaused ->
            Button.view model.finishButton

        CallUnpaused ->
            Button.view model.finishButton

        PresentStarted ->
            Button.view model.finishButton

        PresentStopped ->
            Button.view model.finishButton

        FinishAttempt ->
            Button.view model.finishButton

        _ ->
            emptyHtml


presentButton : { model | presentButton : Button.Config msg, state : CallState } -> Html msg
presentButton model =
    case model.state of
        CallStarted ->
            Button.view model.presentButton

        CallUnpaused ->
            Button.view model.presentButton

        PresentAttempt ->
            Button.view model.presentButton

        PresentStarted ->
            Button.view model.presentButton

        StopPresentAttempt ->
            Button.view model.presentButton

        PresentStopped ->
            Button.view model.presentButton

        _ ->
            emptyHtml
