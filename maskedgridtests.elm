import MaskedGrid exposing (..)
import Mask
import Grid
import Sidewinder

import String
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)
import Html exposing (..)
import Random exposing (..)

createGrid rows cols =
    createMaskedGrid (Mask.createMask rows cols) (Random.initialSeed 123)


gridTests : Test
gridTests = suite "Masked grid test suite"
    [ test "Masked Grid size equals grid size" (
        let grid = createGrid 3 3
        in
           assertEqual (MaskedGrid.size (grid)) 9)
    , test "Can be used with maze algorithms" (
        let grid = createGrid 3 3
        in
           assert (not <| (Grid.size (Sidewinder.on grid) == 9)))
    ]

main = runDisplay gridTests
