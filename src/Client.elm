module Client exposing
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

{-| The interface to the Client config structure.
This includes:

  - The prefered technology type

-}

import Api
import Api.Endpoint as Endpoint
import Http
import Json.Decode as Decode
    exposing
        ( Decoder
        , bool
        , int
        , list
        , string
        , succeed
        )
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode



-- TYPES


type alias ClientConfig =
    { technologies : List Technology
    , type_ : ClientType
    }


type alias AcknowledgmentMetadata =
    { id : Int
    , technology : Technology
    , mobileFlag : Bool
    , state : CallState
    }


type CallState
    = Initial
    | LoadingConfig
    | ConfigLoaded
    | StartAttempt
    | CallStarted
    | PauseAttempt
    | CallPaused
    | UnpauseAttempt
    | CallUnpaused
    | FinishAttempt
    | CallFinished
    | PresentAttempt
    | PresentStarted
    | StopPresentAttempt
    | PresentStopped
    | Failed


type ClientType
    = Presenter
    | Guest
    | Unknown


type Technology
    = VNC
    | WebRTC
    | AnyOther


type SharingOption
    = Screen
    | Window
    | Mobile



-- SERIALIZATION


configDecoder : Decoder ClientConfig
configDecoder =
    Decode.succeed ClientConfig
        |> required "technologies" technologiesDecoder
        |> required "presenter" clientTypeDecoder


technologiesDecoder : Decoder (List Technology)
technologiesDecoder =
    Decode.list technologyDecoder


technologyDecoder : Decoder Technology
technologyDecoder =
    Decode.string
        |> Decode.andThen
            (\b ->
                case b of
                    "VNC" ->
                        Decode.succeed VNC

                    "WebRTC" ->
                        Decode.succeed WebRTC

                    "AnyOther" ->
                        Decode.succeed AnyOther

                    other ->
                        Decode.fail <| "Unknown technology: " ++ other
            )


clientTypeDecoder : Decoder ClientType
clientTypeDecoder =
    Decode.bool
        |> Decode.andThen
            (\b ->
                if b then
                    Decode.succeed Presenter

                else
                    Decode.succeed Guest
            )


acknowledgeDecoder : Decoder AcknowledgmentMetadata
acknowledgeDecoder =
    Decode.succeed AcknowledgmentMetadata
        |> required "id" int
        |> required "technology" technologyDecoder
        |> required "mobileFlag" bool
        |> required "state" stateDecoder


stateDecoder : Decoder CallState
stateDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Initial" ->
                        Decode.succeed Initial

                    "StartAttempt" ->
                        Decode.succeed StartAttempt

                    "PauseAttempt" ->
                        Decode.succeed PauseAttempt

                    "UnpauseAttempt" ->
                        Decode.succeed UnpauseAttempt

                    "FinishAttempt" ->
                        Decode.succeed FinishAttempt

                    "PresentAttempt" ->
                        Decode.succeed PresentAttempt

                    "StopPresentAttempt" ->
                        Decode.succeed StopPresentAttempt

                    "CallStarted" ->
                        Decode.succeed CallStarted

                    "CallPaused" ->
                        Decode.succeed CallPaused

                    "CallUnpaused" ->
                        Decode.succeed CallUnpaused

                    "CallFinished" ->
                        Decode.succeed CallFinished

                    "PresentStarted" ->
                        Decode.succeed PresentStarted

                    "PresentStopped" ->
                        Decode.succeed PresentStopped

                    "LoadingConfig" ->
                        Decode.succeed LoadingConfig

                    "Failed" ->
                        Decode.succeed Failed

                    other ->
                        Decode.fail <| "Unknown technology: " ++ other
            )



-- API calls


getConfig : { mobileFlag : Bool, rtcPriority : Bool, error : Bool } -> Http.Request ClientConfig
getConfig config =
    Api.get (Endpoint.clientConfig config) configDecoder


acknowledgeRequest :
    { technology : Technology
    , state : CallState
    , mobileFlag : Bool
    , sharingOption : Maybe SharingOption
    }
    -> Http.Request AcknowledgmentMetadata
acknowledgeRequest { technology, state, mobileFlag, sharingOption } =
    let
        body =
            Encode.object
                [ ( "technology", Encode.string <| technologyToString technology )
                , ( "state", Encode.string <| stateToString state )
                , ( "mobileFlag", Encode.bool mobileFlag )
                , ( "sharingOption", Encode.string <| sharingOptionToString sharingOption )
                ]
                |> Http.jsonBody
    in
    Api.post Endpoint.acknowledge body acknowledgeDecoder



-- HELPERS


clientTypeFromString : Maybe String -> ClientType
clientTypeFromString clientTypeStr =
    case clientTypeStr of
        Nothing ->
            Presenter

        Just u ->
            case u of
                "guest" ->
                    Guest

                "presenter" ->
                    Presenter

                _ ->
                    Unknown


clientTypeToString : ClientType -> String
clientTypeToString clientType =
    case clientType of
        Presenter ->
            "Presenter"

        Guest ->
            "Guest"

        Unknown ->
            "Loading"


technologyToString : Technology -> String
technologyToString tech =
    case tech of
        VNC ->
            "VNC"

        WebRTC ->
            "WebRTC"

        AnyOther ->
            "Loading"


stateToString : CallState -> String
stateToString state =
    case state of
        Initial ->
            "Initial"

        LoadingConfig ->
            "LoadingConfig"

        Failed ->
            "Failed"

        CallStarted ->
            "CallStarted"

        CallPaused ->
            "CallPaused"

        CallUnpaused ->
            "CallUnpaused"

        CallFinished ->
            "CallFinished"

        PresentStarted ->
            "PresentStarted"

        PresentStopped ->
            "PresentStopped"

        StartAttempt ->
            "StartAttempt"

        PauseAttempt ->
            "PauseAttempt"

        UnpauseAttempt ->
            "UnpauseAttempt"

        FinishAttempt ->
            "FinishAttempt"

        PresentAttempt ->
            "PresentAttempt"

        StopPresentAttempt ->
            "StopPresentAttempt"

        ConfigLoaded ->
            "ConfigLoaded"


sharingOptionToString : Maybe SharingOption -> String
sharingOptionToString maybeOption =
    case maybeOption of
        Nothing ->
            ""

        Just option ->
            case option of
                Window ->
                    "Window Sharing"

                Screen ->
                    "Screen Sharing"

                Mobile ->
                    "Mobile Display"
