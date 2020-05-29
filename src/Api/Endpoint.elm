module Api.Endpoint exposing
    ( Endpoint
    , acknowledge
    , clientConfig
    , request
    )

import Http
import Url.Builder exposing (QueryParameter)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , withCredentials : Bool
    }
    -> Http.Request a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , withCredentials = config.withCredentials
        }



-- TYPES


type Endpoint
    = Endpoint String


type Api
    = MockServer


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : Api -> List String -> List QueryParameter -> Endpoint
url api paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    let
        apiRef =
            case api of
                MockServer ->
                    "http://localhost:3000"
    in
    Url.Builder.crossOrigin apiRef paths queryParams
        |> Endpoint



-- ENDPOINTS
-- retrieves the config for each different case we control in this application
-- if it's mobile, RTC first, Mobile, etc


clientConfig : { mobileFlag : Bool, rtcPriority : Bool } -> Endpoint
clientConfig { mobileFlag, rtcPriority } =
    case ( rtcPriority, mobileFlag ) of
        ( True, _ ) ->
            -- clients/3 config in db.json
            url MockServer [ "clients", "3" ] []

        ( _, True ) ->
            -- clients/2 config in db.json
            url MockServer [ "clients", "2" ] []

        _ ->
            -- clients/1 config in db.json
            url MockServer [ "clients", "1" ] []



-- posts actions to the API according to the flow


acknowledge : Endpoint
acknowledge =
    url MockServer [ "acknowledgments" ] []
