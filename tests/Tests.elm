module Tests exposing (..)

import Test exposing (..)
import MatrixTests


all : Test
all =
    describe "Sample Test Suite"
        [ MatrixTests.all
        ]
