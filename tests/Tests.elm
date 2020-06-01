module Tests exposing (all)

import PagesTests
import RoutingTests
import Test exposing (..)
import TimestampTests


all : Test
all =
    describe "App"
        [ PagesTests.all
        , RoutingTests.fromUrl
        , TimestampTests.all
        ]
