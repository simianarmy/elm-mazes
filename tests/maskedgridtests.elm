module Main exposing (createGrid, gridTests, main)

import Cell
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)
import ElmTest.Test exposing (Test, suite, test)
import Grid
import Html exposing (..)
import Mask
import MaskedGrid exposing (..)
import Random exposing (..)
import Sidewinder
import String


createGrid rows cols =
    MaskedGrid.createGrid (Mask.createMask rows cols) (Random.initialSeed 123)


gridTests : Test
gridTests =
    suite "Masked grid test suite"
        [ test "Masked Grid size equals grid size"
            (let
                grid =
                    createGrid 3 3
             in
             assertEqual (MaskedGrid.size grid) 9
            )
        , test "Can be used with maze algorithms"
            (let
                grid =
                    createGrid 3 3

                grid' =
                    Sidewinder.on <| grid
             in
             assert (not <| List.isEmpty <| Grid.deadEnds grid')
            )
        , test "Counts number of masked bits"
            (let
                mask =
                    Mask.createMask 4 4

                mask' =
                    Mask.set mask ( 1, 1 ) False
             in
             assertEqual (Mask.count mask') 15
            )
        , test "Returns boolean status of a bit at any position"
            (let
                mask =
                    Mask.createMask 4 4

                mask' =
                    Mask.set mask ( 2, 2 ) False
             in
             assert (not <| Mask.get mask' 2 2)
            )
        , test "Masked grid returns off bit as nil cell"
            (let
                mask =
                    Mask.createMask 3 3

                mask' =
                    Mask.set mask ( 2, 2 ) False

                grid =
                    MaskedGrid.createGrid mask' (Random.initialSeed 123)
             in
             assert (Cell.isNil <| Grid.getCell grid 2 2)
            )
        ]


main =
    runDisplay gridTests
