module Page.Components.ButtonTests exposing (all)

import Expect exposing (Expectation)
import Test exposing (..)



-- shame, shame, shame! no tests for this module


testName : String
testName =
    "ButtonTests"


all : Test
all =
    test "ButtonTests" (\_ -> Expect.equal "ButtonTests" testName)
