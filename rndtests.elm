-- rndtests.elm
import Grid exposing (..)
import Rnd exposing (..)

import Random
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)
import Html exposing (..)

rndTests : Test
rndTests = suite "Random number generator test suite"
        [ test "Grid Rnd creator" (
            let rows = 2
                cols = 2
                rnd = createGridRnd rows cols (Random.initialSeed 123)
                rnd' = Rnd.refresh rnd
                row = rnd'.row
                col = rnd'.col
            in assert (row >= 1 && row <= rows && col >= 1 && col <= cols))
        ]

main = runDisplay rndTests

