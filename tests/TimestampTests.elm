module TimestampTests exposing (all)

import Expect exposing (Expectation)
import Html.Styled as Styled exposing (Html)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector
    exposing
        ( Selector
        , tag
        , text
        )
import Time
import Timestamp


all : Test
all =
    describe "Timestamp"
        [ viewTests
        ]


viewTests : Test
viewTests =
    describe "view"
        [ test "when date is Nothing" <|
            \_ ->
                Timestamp.view Time.utc Nothing
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.has [ text "--" ]
        , test "when date is something" <|
            \_ ->
                Timestamp.view Time.utc (Just <| Time.millisToPosix 1554130556150)
                    |> Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.has [ text "April 1, 2019" ]
        ]
