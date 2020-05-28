module Client exposing
    ( ClientConfig
    , ClientType(..)
    , Technology(..)
    , clientTypeFromString
    , clientTypeToString
    , getConfig
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
        , list
        , string
        , succeed
        )
import Json.Decode.Pipeline exposing (required)



-- TYPES


type alias ClientConfig =
    { technologies : List Technology
    , type_ : ClientType
    }


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



-- API


getConfig : Http.Request ClientConfig
getConfig =
    Api.get Endpoint.clientConfig configDecoder



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
