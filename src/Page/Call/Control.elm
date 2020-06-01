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


{-| this module abstraction handles the display
of the buttons for the call
-}



--initial Button states


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


startButton :
    { model
        | startButton : Button.Config msg
        , state : CallState
    }
    -> Html msg
startButton model =
    let
        allowedStates =
            [ LoadingConfig
            , ConfigLoaded
            , StartAttempt
            , CallFinished
            , Failed
            ]
    in
    --better than casing here, to keep it DRY
    if List.member model.state allowedStates then
        Button.view model.startButton

    else
        emptyHtml


pauseButton :
    { model
        | pauseButton : Button.Config msg
        , state : CallState
    }
    -> Html msg
pauseButton model =
    let
        allowedStates =
            [ CallStarted
            , CallPaused
            , PauseAttempt
            , CallUnpaused
            , UnpauseAttempt
            , PresentStarted
            , PresentStopped
            ]
    in
    if List.member model.state allowedStates then
        Button.view model.pauseButton

    else
        emptyHtml


finishButton :
    { model
        | finishButton : Button.Config msg
        , state : CallState
    }
    -> Html msg
finishButton model =
    let
        allowedStates =
            [ CallStarted
            , CallPaused
            , CallUnpaused
            , PresentStarted
            , PresentStopped
            , FinishAttempt
            ]
    in
    if List.member model.state allowedStates then
        Button.view model.finishButton

    else
        emptyHtml


presentButton : { model | presentButton : Button.Config msg, state : CallState } -> Html msg
presentButton model =
    let
        allowedStates =
            [ CallStarted
            , CallUnpaused
            , PresentAttempt
            , PresentStarted
            , StopPresentAttempt
            , PresentStopped
            ]
    in
    if List.member model.state allowedStates then
        Button.view model.presentButton

    else
        emptyHtml
