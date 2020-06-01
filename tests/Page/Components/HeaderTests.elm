module Page.Components.HeaderTests exposing (all)

import Expect exposing (Expectation)
import Test exposing (..)



-- shame, shame, shame! no tests for this module


testName : String
testName =
    "HeaderTests"


all : Test
all =
    test "HeaderTests" (\_ -> Expect.equal "HeaderTests" testName)
