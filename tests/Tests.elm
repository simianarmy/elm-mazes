module Tests exposing (..)

import BinaryTreeTests
import Expect
import Fuzz exposing (int, list, string, tuple)
import GridCellTests
import GridTests
import String
import Test exposing (..)


all : Test
all =
    describe "Sample Test Suite"
        [ GridCellTests.all
        , GridTests.all
        , BinaryTreeTests.all
        ]
