-- mazetests.elm
import Maze exposing (..)

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)
import Html exposing (..)
import Random exposing (..)

mazeTests : Test
mazeTests = suite "Maze test suite"
[ test "initializing creates grid" ()
, test "initializing saves algorithm" (),
, test "generates distances from a cell" ()
]
