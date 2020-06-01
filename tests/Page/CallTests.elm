module Page.CallTests exposing (all)

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
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (int, list, string)
import Html
import Html.Styled as Styled exposing (Html)
import Http exposing (Error, Response)
import Page.Call as Call
    exposing
        ( ConfigStatus(..)
        , Msg(..)
        , Problem(..)
        )
import Page.Call.Control as Control
import Page.Components.Button as Button exposing (Config, State(..))
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector
    exposing
        ( Selector
        , tag
        , text
        )
import Time
import Utils.Chrono as Chrono



--  HELPERS


pageContent :
    { state : CallState
    , showScreenDialog : Bool
    , sharingOption : Maybe SharingOption
    , mobileFlag : Bool
    , asGuest : Bool
    , webRTC : Bool
    }
    -> Html Call.Msg
pageContent { state, showScreenDialog, sharingOption, mobileFlag, asGuest, webRTC } =
    let
        config =
            setConfig asGuest webRTC

        newMock =
            { modelMock
                | state = state
                , showScreenDialog = showScreenDialog
                , sharingOption = sharingOption
                , mobileFlag = mobileFlag
                , clientConfig = ConfigSuccess config
            }

        { title, content } =
            Call.view newMock
    in
    content


type alias ModelWithoutSession =
    { title : String
    , problems : List Problem
    , timeZone : Time.Zone
    , chrono : Chrono.Model
    , mobileFlag : Bool
    , clientConfig : ConfigStatus ClientConfig
    , sharingOption : Maybe SharingOption
    , state : CallState
    , showScreenDialog : Bool
    , startButton : Config Msg
    , finishButton : Config Msg
    , pauseButton : Config Msg
    , presentButton : Config Msg
    }


modelMock : ModelWithoutSession
modelMock =
    let
        ( chronoModel, _ ) =
            Chrono.init
    in
    { title = "Snapview call"
    , problems = []
    , timeZone = Time.utc
    , chrono = chronoModel
    , mobileFlag = False
    , clientConfig = ConfigSuccess mockConfigLoaded
    , sharingOption = Nothing
    , state = ConfigLoaded
    , showScreenDialog = False
    , startButton = Control.initialStartButton StartCall
    , finishButton = Control.initialFinishButton FinishCall
    , pauseButton = Control.initialPauseButton PauseCall
    , presentButton = Control.initialPresentButton OpenScreenShareModal
    }


mockConfigLoaded : Client.ClientConfig
mockConfigLoaded =
    { technologies = [ Client.VNC, Client.WebRTC ]
    , type_ = Client.Presenter
    }


setConfig : Bool -> Bool -> Client.ClientConfig
setConfig asGuest webRTC =
    let
        config =
            if asGuest then
                { mockConfigLoaded | type_ = Client.Guest }

            else
                mockConfigLoaded

        updatedConfig =
            if webRTC then
                { config | technologies = [ Client.WebRTC, Client.VNC ] }

            else
                config
    in
    updatedConfig



-- TESTS


all : Test
all =
    describe "Call"
        [ viewTests

        --,updateTests
        ]


viewTests : Test
viewTests =
    describe "view"
        [ -- scenario 1 Presenter
          test "has the correct messages when loading" <|
            \_ ->
                pageContent
                    { state = LoadingConfig
                    , showScreenDialog = False
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ text "LoadingConfig..." ]
                        , Query.has [ text "ClientType: Presenter" ]
                        , Query.has [ text "Tech Type: VNC" ]
                        ]
        , test "has the correct amount of buttons when loading" <|
            \_ ->
                pageContent
                    { state = LoadingConfig
                    , showScreenDialog = False
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.count (Expect.equal 1)
        , test "has the correct messages when loaded" <|
            \_ ->
                pageContent
                    { state = ConfigLoaded
                    , showScreenDialog = False
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ text "ConfigLoaded" ]
                        , Query.has [ text "ClientType: Presenter" ]
                        , Query.has [ text "Tech Type: VNC" ]
                        ]
        , test "has the correct amount of buttons when loaded" <|
            \_ ->
                pageContent
                    { state = ConfigLoaded
                    , showScreenDialog = False
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.count (Expect.equal 1)
        , test "has the correct messages when starting" <|
            \_ ->
                pageContent
                    { state = StartAttempt
                    , showScreenDialog = False
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ text "Starting..." ]
                        , Query.has [ text "ClientType: Presenter" ]
                        , Query.has [ text "Tech Type: VNC" ]
                        ]
        , test "has the correct amount of buttons when starting" <|
            \_ ->
                pageContent
                    { state = StartAttempt
                    , showScreenDialog = False
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.count (Expect.equal 1)
        , test "has the correct messages when started" <|
            \_ ->
                pageContent
                    { state = CallStarted
                    , showScreenDialog = False
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ text "You're sharing your camera and it's off..." ]
                        , Query.has [ text "ClientType: Presenter" ]
                        , Query.has [ text "Tech Type: VNC" ]
                        ]
        , test "has the correct amount of buttons when started" <|
            \_ ->
                pageContent
                    { state = CallStarted
                    , showScreenDialog = False
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.count (Expect.equal 3)
        , test "displays the sharing modal" <|
            \_ ->
                pageContent
                    { state = CallStarted
                    , showScreenDialog = True
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ text "You're sharing your camera and it's off..." ]
                        , Query.has [ text "ClientType: Presenter" ]
                        , Query.has [ text "Select Screen" ]
                        , Query.has [ text "Select Window" ]
                        , Query.has [ text "Tech Type: VNC" ]
                        ]
        , test "has the correct message when presenting a" <|
            \_ ->
                pageContent
                    { state = PresentStarted
                    , showScreenDialog = False
                    , sharingOption = Just Screen
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ text "You are Screen Sharing" ]
                        , Query.has [ text "ClientType: Presenter" ]
                        , Query.has [ text "Tech Type: VNC" ]
                        ]
        , test "has the correct message when presenting b" <|
            \_ ->
                pageContent
                    { state = PresentStarted
                    , showScreenDialog = False
                    , sharingOption = Just Window
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ text "You are Window Sharing" ]
                        , Query.has [ text "ClientType: Presenter" ]
                        , Query.has [ text "Tech Type: VNC" ]
                        ]
        , test "has the correct amount of buttons when presenting" <|
            \_ ->
                pageContent
                    { state = PresentStarted
                    , showScreenDialog = False
                    , sharingOption = Just Window
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.count (Expect.equal 3)
        , test "has the correct message when pausing" <|
            \_ ->
                pageContent
                    { state = CallPaused
                    , showScreenDialog = False
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ text "ClientType: Presenter" ]
                        , Query.has [ text "Tech Type: VNC" ]
                        ]
        , test "has the correct amount of buttons when pausing" <|
            \_ ->
                pageContent
                    { state = CallPaused
                    , showScreenDialog = False
                    , sharingOption = Just Window
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.count (Expect.equal 2)
        , test "has the correct message when finishing" <|
            \_ ->
                pageContent
                    { state = FinishAttempt
                    , showScreenDialog = False
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ text "Finishing call..." ]
                        , Query.has [ text "ClientType: Presenter" ]
                        , Query.has [ text "Tech Type: VNC" ]
                        ]
        , test "has the correct amount of buttons when finishing" <|
            \_ ->
                pageContent
                    { state = FinishAttempt
                    , showScreenDialog = False
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.count (Expect.equal 1)

        -- scenario 2 guest and mobileTrue
        , test "has the correct message when starting a mobile call" <|
            \_ ->
                pageContent
                    { state = CallStarted
                    , showScreenDialog = False
                    , sharingOption = Just Mobile
                    , mobileFlag = True
                    , asGuest = True
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ text "You are Mobile Display" ]
                        , Query.has [ text "ClientType: Guest" ]
                        , Query.has [ text "Tech Type: VNC" ]
                        ]
        , test "has the correct amount of buttons when starting a mobile call" <|
            \_ ->
                pageContent
                    { state = CallStarted
                    , showScreenDialog = False
                    , sharingOption = Just Mobile
                    , mobileFlag = True
                    , asGuest = True
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.count (Expect.equal 2)

        -- scenario 3 presenter and WebRTC
        , test "has the correct message when starting a webRTC call" <|
            \_ ->
                pageContent
                    { state = CallStarted
                    , showScreenDialog = True
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = True
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ text "You're sharing your camera and it's off..." ]
                        , Query.has [ text "ClientType: Presenter" ]
                        , Query.has [ text "Tech Type: WebRTC" ]
                        , Query.has [ text "Select Screen" ]
                        , Query.hasNot [ text "Select Window" ]
                        ]
        , test "has the correct amount of buttons when starting a webRTC call" <|
            \_ ->
                pageContent
                    { state = CallStarted
                    , showScreenDialog = False
                    , sharingOption = Just Screen
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = True
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.count (Expect.equal 3)

        -- scenario 4 error and retry next technology
        , test "has the correct message when received a failed state" <|
            \_ ->
                pageContent
                    { state = Failed
                    , showScreenDialog = False
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ text "Failed... Retrying..." ]
                        , Query.has [ text "ClientType: Presenter" ]
                        , Query.has [ text "Tech Type: VNC" ]
                        ]
        , test "has the correct amount of buttons when received a failed state" <|
            \_ ->
                pageContent
                    { state = Failed
                    , showScreenDialog = False
                    , sharingOption = Nothing
                    , mobileFlag = False
                    , asGuest = False
                    , webRTC = False
                    }
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.count (Expect.equal 1)
        ]
