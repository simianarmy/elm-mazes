-- binarytreetests.elm

import BinaryTree exposing (..)
import Grid exposing (..)
import Cell exposing (..)

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)
import Html exposing (..)
import Set
import Random

binarytreeTests : Test
binarytreeTests = suite "Binary Tree Algorithm test suite"
    [ test "Grid cells are of equal length" (
        let grid = createGrid 3 3
            seed = Random.initialSeed 123123
        in
            assertEqual (List.length grid.cells) (List.length (on grid seed).cells))
    , test "Cell links are created" (
        let grid = on (createGrid 3 3) (Random.initialSeed 123123)
        in
            assertEqual (List.isEmpty (List.filter (\a -> not (Set.isEmpty (linked a))) grid.cells)) False)
            ]

main = runDisplay binarytreeTests
