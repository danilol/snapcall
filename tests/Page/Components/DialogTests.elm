module Page.Components.DialogTests exposing (all)

import Expect exposing (Expectation)
import Test exposing (..)



-- shame, shame, shame! no tests for this module


testName : String
testName =
    "DialogTests"


all : Test
all =
    test "DialogTests" (\_ -> Expect.equal "DialogTests" testName)
