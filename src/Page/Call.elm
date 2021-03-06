module Page.Call exposing
    ( ConfigStatus(..)
    , Model
    , Msg(..)
    , Problem(..)
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

{-| The call page. You can get here via the /call route.
-}

import Api exposing (decodeError)
import Client
    exposing
        ( AcknowledgmentMetadata
        , CallState(..)
        , ClientConfig
        , ClientType(..)
        , SharingOption(..)
        , Technology(..)
        , acknowledgeRequest
        , clientTypeToString
        , getConfig
        , sharingOptionToString
        , stateToString
        , technologyToString
        )
import Html.Styled as Styled
    exposing
        ( Html
        , div
        , h1
        , map
        , span
        , text
        )
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Http
import Page.Call.Control as Control
import Page.Call.ScenarioConfig as ScenarioConfig
import Page.Call.Styles
    exposing
        ( callContainer
        , camera
        , cameraContainter
        , sharingOptionContainer
        , sharingOptionContent
        )
import Page.Components.Button as Button exposing (Config, State(..))
import Page.Components.Dialog as Dialog
import Page.Components.Header as Header
import Session exposing (Session)
import SharedStyles
import Time
import Utils.Chrono as Chrono
import Utils.Html exposing (emptyHtml)



-- MODEL


type alias Model =
    { title : String
    , session : Session
    , problems : List Problem
    , timeZone : Time.Zone
    , chrono : Chrono.Model
    , timeCallStarted : Maybe Time.Posix

    -- config
    , mobileFlag : Bool
    , clientConfig : ConfigStatus ClientConfig
    , sharingOption : Maybe SharingOption

    --screen control
    , state : CallState
    , showScreenDialog : Bool
    , startButton : Config Msg
    , finishButton : Config Msg
    , pauseButton : Config Msg
    , presentButton : Config Msg
    }


type Problem
    = ServerError String


type ConfigStatus a
    = ConfigInitial
    | ConfigSuccess a
    | ConfigFailed


init : Session -> Maybe String -> ( Model, Cmd Msg )
init session scenarioParam =
    let
        scenarioConfig =
            ScenarioConfig.prepare scenarioParam

        -- if mobileFlag, starts directly
        sharingOption =
            if scenarioConfig.mobileFlag then
                Just Mobile

            else
                Nothing

        ( chronoModel, chronoCmd ) =
            Chrono.init
    in
    ( { title = "Snapview call"
      , session = session
      , problems = []
      , timeZone = Time.utc
      , chrono = chronoModel
      , timeCallStarted = Nothing

      -- config
      , mobileFlag = scenarioConfig.mobileFlag
      , clientConfig = ConfigInitial
      , sharingOption = sharingOption

      -- screen control
      , state = LoadingConfig
      , showScreenDialog = False
      , startButton = Control.initialStartButton StartCall
      , pauseButton = Control.initialPauseButton PauseCall
      , finishButton = Control.initialFinishButton FinishCall
      , presentButton = Control.initialPresentButton OpenScreenShareModal
      }
    , Cmd.batch
        [ Http.send RequestConfig <| getConfig scenarioConfig
        , Cmd.map GotChronoMsg chronoCmd
        ]
    )



-- UPDATE


type Msg
    = RequestConfig (Result Http.Error ClientConfig)
    | Acknowledge (Result Http.Error AcknowledgmentMetadata)
    | OpenScreenShareModal
    | CloseScreenShareModal
    | StartSharing SharingOption
    | StartCall
    | PauseCall
    | UnpauseCall
    | FinishCall
    | StopSharing
    | GotChronoMsg Chrono.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestConfig (Ok config) ->
            let
                newStartBtn =
                    if model.mobileFlag then
                        updateButtonState
                            { oldBtn = model.startButton
                            , newState = Button.Loading
                            , newLabel = model.startButton.label
                            , onClick = model.startButton.onClick
                            }

                    else
                        updateButtonState
                            { oldBtn = model.startButton
                            , newState = Button.Enabled
                            , newLabel = model.startButton.label
                            , onClick = model.startButton.onClick
                            }

                modelWithNewButton =
                    { model
                        | startButton = newStartBtn
                        , clientConfig = ConfigSuccess config
                    }

                ( updatedModel, updatedCmd ) =
                    -- if mobileFlag starts directly
                    if model.mobileFlag then
                        update StartCall modelWithNewButton

                    else
                        ( { model
                            | startButton = newStartBtn
                            , clientConfig = ConfigSuccess config
                            , state = Client.ConfigLoaded
                          }
                        , Cmd.none
                        )
            in
            ( updatedModel, updatedCmd )

        RequestConfig (Err error) ->
            let
                serverErrors =
                    [ ServerError (decodeError error) ]

                newStartBtn =
                    updateButtonState
                        { oldBtn = model.startButton
                        , newState = Button.Enabled
                        , newLabel = model.startButton.label
                        , onClick = model.startButton.onClick
                        }
            in
            ( { model
                | clientConfig = ConfigFailed
                , problems = serverErrors
                , startButton = newStartBtn
                , state = Failed
              }
            , Cmd.none
            )

        StartCall ->
            let
                state =
                    StartAttempt

                newStartBtn =
                    updateButtonState
                        { oldBtn = model.startButton
                        , newState = Button.Loading
                        , newLabel = model.startButton.label
                        , onClick = model.startButton.onClick
                        }
            in
            ( { model
                | state = state
                , startButton = newStartBtn
              }
            , Cmd.batch
                [ acknowledgeCmd
                    { technology = extractTechnology model.clientConfig
                    , sharingOption = model.sharingOption
                    , state = state
                    , mobileFlag = model.mobileFlag
                    }
                ]
            )

        PauseCall ->
            let
                state =
                    PauseAttempt

                newPauseBtn =
                    updateButtonState
                        { oldBtn = model.pauseButton
                        , newState = Button.Loading
                        , newLabel = "Unpause"
                        , onClick = UnpauseCall
                        }
            in
            ( { model | state = state, pauseButton = newPauseBtn }
            , acknowledgeCmd
                { technology = extractTechnology model.clientConfig
                , sharingOption = model.sharingOption
                , state = state
                , mobileFlag = model.mobileFlag
                }
            )

        UnpauseCall ->
            let
                state =
                    UnpauseAttempt

                newPauseBtn =
                    updateButtonState
                        { oldBtn = model.pauseButton
                        , newState = Button.Loading
                        , newLabel = "Pause"
                        , onClick = PauseCall
                        }
            in
            ( { model | state = state, pauseButton = newPauseBtn }
            , acknowledgeCmd
                { technology = extractTechnology model.clientConfig
                , mobileFlag = model.mobileFlag
                , state = state
                , sharingOption = model.sharingOption
                }
            )

        FinishCall ->
            let
                state =
                    FinishAttempt

                newFinishButton =
                    updateButtonState
                        { oldBtn = model.finishButton
                        , newState = Button.Loading
                        , newLabel = model.finishButton.label
                        , onClick = model.finishButton.onClick
                        }
            in
            ( { model
                | state = state
                , pauseButton = Control.initialPauseButton PauseCall
                , presentButton = Control.initialPresentButton OpenScreenShareModal
                , finishButton = newFinishButton
                , sharingOption = Nothing
              }
            , acknowledgeCmd
                { technology = extractTechnology model.clientConfig
                , sharingOption = model.sharingOption
                , state = state
                , mobileFlag = model.mobileFlag
                }
            )

        StartSharing opt ->
            let
                newSharingOption =
                    Just opt

                state =
                    PresentAttempt

                newPresentButton =
                    updateButtonState
                        { oldBtn = model.presentButton
                        , newState = Button.Loading
                        , newLabel = "Stop Sharing"
                        , onClick = model.presentButton.onClick
                        }
            in
            ( { model
                | sharingOption = newSharingOption
                , showScreenDialog = False
                , presentButton = newPresentButton
                , state = state
              }
            , acknowledgeCmd
                { technology = extractTechnology model.clientConfig
                , mobileFlag = model.mobileFlag
                , state = state
                , sharingOption = newSharingOption
                }
            )

        StopSharing ->
            let
                state =
                    StopPresentAttempt

                newPresentButton =
                    updateButtonState
                        { oldBtn = model.presentButton
                        , newState = Button.Loading
                        , newLabel = "Present Now"
                        , onClick = OpenScreenShareModal
                        }
            in
            ( { model
                | sharingOption = Nothing
                , showScreenDialog = False
                , presentButton = newPresentButton
                , state = state
              }
            , acknowledgeCmd
                { technology = extractTechnology model.clientConfig
                , mobileFlag = model.mobileFlag
                , state = state
                , sharingOption = Nothing
                }
            )

        Acknowledge (Ok acknowledge) ->
            let
                newState =
                    case acknowledge.state of
                        StartAttempt ->
                            CallStarted

                        PauseAttempt ->
                            CallPaused

                        UnpauseAttempt ->
                            CallUnpaused

                        FinishAttempt ->
                            CallFinished

                        PresentAttempt ->
                            PresentStarted

                        StopPresentAttempt ->
                            PresentStopped

                        _ ->
                            acknowledge.state

                newStartBtn =
                    updateButtonState
                        { oldBtn = model.startButton
                        , newState = Button.Enabled
                        , newLabel = model.startButton.label
                        , onClick = model.startButton.onClick
                        }

                newPauseBtn =
                    updateButtonState
                        { oldBtn = model.pauseButton
                        , newState = Button.Enabled
                        , newLabel = model.pauseButton.label
                        , onClick =
                            if newState == PauseAttempt then
                                UnpauseCall

                            else
                                model.pauseButton.onClick
                        }

                newFinishButton =
                    updateButtonState
                        { oldBtn = model.finishButton
                        , newState = Button.Enabled
                        , newLabel = model.finishButton.label
                        , onClick = model.finishButton.onClick
                        }

                newPresentButton =
                    updateButtonState
                        { oldBtn = model.presentButton
                        , newState = Button.Enabled
                        , newLabel = model.presentButton.label
                        , onClick =
                            if newState == PresentStarted then
                                StopSharing

                            else
                                model.presentButton.onClick
                        }

                ( chronoModel, chronoCmd ) =
                    case acknowledge.state of
                        StartAttempt ->
                            Chrono.start model.chrono

                        FinishAttempt ->
                            Chrono.stop model.chrono

                        _ ->
                            ( model.chrono, Cmd.none )
            in
            ( { model
                | state = newState
                , startButton = newStartBtn
                , pauseButton = newPauseBtn
                , presentButton = newPresentButton
                , finishButton = newFinishButton
                , chrono = chronoModel
              }
            , Cmd.map GotChronoMsg chronoCmd
            )

        Acknowledge (Err error) ->
            let
                serverErrors =
                    [ ServerError (decodeError error) ]

                -- work around for retry
                -- if config request was successfull that means we have
                -- the tecnologies, so we can try one more time
                ( newClientConfig, retryCmd ) =
                    case model.clientConfig of
                        ConfigSuccess oldConfig ->
                            let
                                --work around to get the next technology of the list
                                newTechnologies =
                                    List.drop 1 oldConfig.technologies

                                newConfig =
                                    ConfigSuccess { oldConfig | technologies = newTechnologies }
                            in
                            ( newConfig
                            , acknowledgeCmd
                                { technology = extractTechnology newConfig
                                , sharingOption = model.sharingOption
                                , state = model.state
                                , mobileFlag = model.mobileFlag
                                }
                            )

                        _ ->
                            ( ConfigFailed, Cmd.none )
            in
            ( { model
                | clientConfig = newClientConfig
                , state = Failed
                , problems = serverErrors
              }
            , retryCmd
            )

        OpenScreenShareModal ->
            ( { model | showScreenDialog = True }, Cmd.none )

        CloseScreenShareModal ->
            ( { model | showScreenDialog = False }, Cmd.none )

        GotChronoMsg chronoMsg ->
            let
                ( chronoModel, chronoCmd ) =
                    Chrono.update chronoMsg model.chrono
            in
            ( { model | chrono = chronoModel }
            , Cmd.map GotChronoMsg chronoCmd
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map GotChronoMsg <| Chrono.subscriptions model.chrono



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



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
        , startButton : Button.Config Msg
        , pauseButton : Button.Config Msg
        , finishButton : Button.Config Msg
        , presentButton : Button.Config Msg
        , mobileFlag : Bool
        , chrono : Chrono.Model
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
        div [ css [ SharedStyles.resetStyles ] ]
            [ Header.view
            , div
                [ css [ callContainer ] ]
                [ div [ css [ SharedStyles.shadow ] ]
                    [ div
                        [ css [ SharedStyles.dataHeader ]
                        ]
                        [ h1 [] [ text <| "Snapview: " ++ stateToString model.state ] ]
                    , div [ css [ cameraContainter ] ]
                        [ div [ css [ camera ] ]
                            [ stateDisplayText model
                            , div [] [ text ("ClientType: " ++ clientTypeToString clientType) ]
                            , div [] [ text ("Tech Type: " ++ technologyToString firstTechnology) ]
                            , Styled.map GotChronoMsg <| Chrono.view model.chrono
                            ]
                        , Control.startButton model
                        , Control.pauseButton model
                        , Control.finishButton model
                        , if model.mobileFlag then
                            emptyHtml

                          else
                            Control.presentButton model
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


stateDisplayText : { model | state : CallState, sharingOption : Maybe SharingOption } -> Html Msg
stateDisplayText model =
    let
        isSharing =
            case model.sharingOption of
                Just _ ->
                    True

                Nothing ->
                    False

        displayText =
            case model.state of
                Initial ->
                    "Click to start the call"

                LoadingConfig ->
                    "LoadingConfig..."

                ConfigLoaded ->
                    "ConfigLoaded"

                CallPaused ->
                    "Transmition Paused..."

                CallFinished ->
                    "Click to start a new call"

                StartAttempt ->
                    "Starting..."

                FinishAttempt ->
                    "Finishing call..."

                Failed ->
                    "Failed... Retrying..."

                _ ->
                    if isSharing then
                        "You are " ++ sharingOptionToString model.sharingOption

                    else
                        "You're sharing your camera and it's off..."
    in
    h1 [ css [] ] [ text displayText ]


screenSharingModalView : Technology -> Html Msg
screenSharingModalView technology =
    case technology of
        WebRTC ->
            div []
                [ span [] [ text "You are about to start Screen sharing:" ]
                , div [ css [ sharingOptionContainer ] ]
                    [ div [ css [ sharingOptionContent ], onClick <| StartSharing Screen ] [ text "Select Screen" ]
                    ]
                ]

        VNC ->
            div []
                [ span [] [ text "You are about to start Screen sharing:" ]
                , div [ css [ sharingOptionContainer ] ]
                    [ div [ css [ sharingOptionContent ], onClick <| StartSharing Screen ] [ text "Select Screen" ]
                    , div [ css [ sharingOptionContent ], onClick <| StartSharing Window ] [ text "Select Window" ]
                    ]
                ]

        AnyOther ->
            emptyHtml



-- HTTP


acknowledgeCmd :
    { technology : Technology
    , sharingOption : Maybe SharingOption
    , state : CallState
    , mobileFlag : Bool
    }
    -> Cmd Msg
acknowledgeCmd { technology, sharingOption, state, mobileFlag } =
    Http.send Acknowledge <|
        acknowledgeRequest
            { technology = technology
            , mobileFlag = mobileFlag
            , state = state
            , sharingOption = sharingOption
            }



-- HELPERS


updateButtonState :
    { oldBtn : Button.Config Msg
    , newState : Button.State
    , newLabel : String
    , onClick : Msg
    }
    -> Button.Config Msg
updateButtonState { oldBtn, newState, newLabel, onClick } =
    { oldBtn | state = newState, label = newLabel, onClick = onClick }


extractTechnology : ConfigStatus ClientConfig -> Technology
extractTechnology config =
    case config of
        ConfigSuccess a ->
            Maybe.withDefault VNC (List.head a.technologies)

        _ ->
            AnyOther


extractClientType : ConfigStatus ClientConfig -> ClientType
extractClientType config =
    case config of
        ConfigSuccess a ->
            a.type_

        _ ->
            Unknown
