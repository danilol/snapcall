module Page.Call.ControlTests exposing (all)

import Expect exposing (Expectation)
import Test exposing (..)



-- shame, shame, shame! no tests for this module


testName : String
testName =
    "ControlTests"


all : Test
all =
    test "ControlTests" (\_ -> Expect.equal "ControlTests" testName)
