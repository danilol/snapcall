module Client exposing
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
    | StartAttempt
    | CallStarted
    | CallStopped
    | Loading
    | Failed


type ClientType
    = Presenter
    | Guest
    | Unknow


type Technology
    = VNC
    | WebRTC



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

                    other ->
                        Decode.fail <| "Unknown technology: " ++ other
            )


clientTypeDecoder : Decoder ClientType
clientTypeDecoder =
    Decode.bool
        |> Decode.andThen
            (\b ->
                case b of
                    True ->
                        Decode.succeed Presenter

                    False ->
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

                    "CallStarted" ->
                        Decode.succeed CallStarted

                    "CallStopped" ->
                        Decode.succeed CallStopped

                    "Loading" ->
                        Decode.succeed Loading

                    "Failed" ->
                        Decode.succeed Failed

                    other ->
                        Decode.fail <| "Unknown technology: " ++ other
            )



-- API


getConfig : Bool -> Http.Request ClientConfig
getConfig option =
    Api.get (Endpoint.clientConfig option) configDecoder


acknowledgeRequest :
    { technology : Technology
    , state : CallState
    , mobileFlag : Bool
    }
    -> Http.Request AcknowledgmentMetadata
acknowledgeRequest config =
    let
        body =
            Encode.object
                [ ( "technology", Encode.string <| technologyToString config.technology )
                , ( "state", Encode.string <| stateToString config.state )
                , ( "mobileFlag", Encode.bool config.mobileFlag )
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
                    Unknow


clientTypeToString : ClientType -> String
clientTypeToString clientType =
    case clientType of
        Presenter ->
            "Presenter"

        Guest ->
            "Guest"

        Unknow ->
            "Something unexpected just happened!"


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
            "CallStarted"

        CallStopped ->
            "CallStopped"

        StartAttempt ->
            "StartAttempt"
