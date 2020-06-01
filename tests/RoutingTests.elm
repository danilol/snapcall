module RoutingTests exposing (fromUrl, testUrl)

import Expect exposing (Expectation)
import Json.Decode as Decode exposing (decodeString)
import Route exposing (Route(..))
import Test exposing (..)
import Url exposing (Url)


fromUrl : Test
fromUrl =
    let
        param =
            "test"
    in
    describe "Route.fromUrl"
        [ testUrl "" Nothing Home
        , testUrl "/call" Nothing (Call Nothing)
        , testUrl "/call" (Just ("scenario=" ++ param)) (Call (Just "test"))
        ]


testUrl : String -> Maybe String -> Route -> Test
testUrl path query route =
    let
        title =
            case query of
                Nothing ->
                    path

                Just q ->
                    path ++ "?" ++ q
    in
    test ("Parsing path: \"" ++ title ++ "\"") <|
        \() ->
            subpath path query
                |> Route.fromUrl
                |> Expect.equal (Just route)


subpath : String -> Maybe String -> Url
subpath path query =
    { protocol = Url.Http
    , host = "foo.com"
    , port_ = Nothing
    , path = path
    , query = query
    , fragment = Nothing
    }
