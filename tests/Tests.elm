module Tests exposing (..)

import GridTests
import GridCellTests
import BinaryTreeTests

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String


all : Test
all =
    describe "Sample Test Suite"
    [
      GridCellTests.all
    , GridTests.all
    , BinaryTreeTests.all
    ]
