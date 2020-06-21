-- rndtests.elm


module Main exposing (main, rndTests)

import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)
import ElmTest.Test exposing (Test, suite, test)
import Grid exposing (..)
import Html exposing (..)
import Random
import Rnd exposing (..)


rndTests : Test
rndTests =
    suite "Random number generator test suite"
        [ test "Grid Rnd creator"
            (let
                rows =
                    2

                cols =
                    2

                rnd =
                    createGridRnd rows cols (Random.initialSeed 123)

                rnd' =
                    Rnd.refresh rnd

                row =
                    rnd'.row

                col =
                    rnd'.col
             in
             assert (row >= 1 && row <= rows && col >= 1 && col <= cols)
            )
        ]


main =
    runDisplay rndTests
