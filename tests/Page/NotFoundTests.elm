module Page.NotFoundTests exposing (all)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (int, list, string)
import Html.Styled as Styled exposing (Html)
import Http exposing (Error, Response)
import Page.NotFound as NotFound
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector, text)



--  HELPERS


pageContent : Html msg
pageContent =
    let
        { title, content } =
            NotFound.view
    in
    content


testContent : Selector -> Expectation
testContent selector =
    Styled.toUnstyled pageContent
        |> Query.fromHtml
        |> Query.has [ selector ]



-- TESTS


all : Test
all =
    describe "NotFound"
        [ viewTests
        ]


viewTests : Test
viewTests =
    describe "view"
        [ test "has the correct Not Found title" <|
            \_ -> testContent (text "Page not found!")
        ]
