module Page.HomeTests exposing (all)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (int, list, string)
import Html.Styled as Styled exposing (Html)
import Http exposing (Error, Response)
import Page.Home as Home
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector
    exposing
        ( Selector
        , tag
        , text
        )



--  HELPERS


pageContent : Html Home.Msg
pageContent =
    let
        { title, content } =
            Home.view { title = "test title" }
    in
    content



-- TESTS


all : Test
all =
    describe "Home"
        [ viewTests
        ]


viewTests : Test
viewTests =
    describe "view"
        [ test "has the correct h1 content" <|
            \_ ->
                Styled.toUnstyled pageContent
                    |> Query.fromHtml
                    |> Query.findAll [ tag "h1" ]
                    |> Query.first
                    |> Query.has [ text "SnapCall" ]
        , test "has the correct h2 content" <|
            \_ ->
                Styled.toUnstyled pageContent
                    |> Query.fromHtml
                    |> Query.findAll [ tag "h2" ]
                    |> Query.first
                    |> Query.has [ text "Snapview code challenge" ]
        , test "has the correct amount of links" <|
            \_ ->
                Styled.toUnstyled pageContent
                    |> Query.fromHtml
                    |> Query.findAll [ tag "a" ]
                    |> Query.count (Expect.equal 1)
        ]
