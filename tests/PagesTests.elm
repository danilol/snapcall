module PagesTests exposing (all)

import Page.CallTests as CallTests
import Page.Components.HeaderTests as HeaderTests
import Page.HomeTests as HomeTests
import Page.NotFoundTests as NotFoundTests
import Test exposing (..)


all : Test
all =
    describe "Pages"
        [ HomeTests.all
        , NotFoundTests.all
        , CallTests.all
        , HeaderTests.all
        ]
