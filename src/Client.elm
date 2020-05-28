module Client exposing
    ( ClientConfig
    , ClientType(..)
    , Technology(..)
    , clientTypeFromString
    , clientTypeToString
    , init
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
        , string
        , succeed
        )
import Json.Decode.Pipeline exposing (required)



-- TYPES


type alias ClientConfig =
    { technology : Technology
    , type_ : ClientType
    }


type ClientType
    = Presenter
    | Guest
    | Unknow


type Technology
    = VNC
    | WebRTC



-- INIT


init : ClientType -> ClientConfig
init type_ =
    { technology = VNC
    , type_ = type_
    }



-- SERIALIZATION
-- API
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


clientTypeToString : Maybe ClientType -> String
clientTypeToString clientType =
    case clientType of
        Nothing ->
            ""

        Just c ->
            case c of
                Presenter ->
                    "Presenter"

                Guest ->
                    "Guest"

                Unknow ->
                    "Something unexpected just happened!"
