-- sidewindertests.elm

import Sidewinder exposing (..)
import Grid exposing (..)
import Cell exposing (..)

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)
import Html exposing (..)
import Set
import Random

sidewinderTests : Test
sidewinderTests = suite "Sidewinder Algorithm test suite"
    [ test "Grid cells are of equal length" (
        let seed = Random.initialSeed 123123
            grid = createGrid 3 3 seed
        in
            assertEqual (List.length grid.cells) (List.length (on grid).cells))
    , test "Cell links are created" (
        let grid = on (createGrid 3 3 (Random.initialSeed 123123))
        in
            assertEqual (List.isEmpty (List.filter (\a -> not (Set.isEmpty (linked a))) grid.cells)) False)
    , test "Cell top row eastern links exist" (
        let grid = on (createGrid 3 3 (Random.initialSeed 123))
        in
           -- top row cells should all have an eastern link
           assert ((Set.member "1:2" (toValidCell (getCell grid 1 1)).links) &&
            (Set.member "1:3" (toValidCell (getCell grid 1 2)).links)))
    , test "Cell east row north/south links exist" (
        let grid = on (createGrid 3 3 (Random.initialSeed 123))
        in
           -- east row cells should all have an southern link
           assert ((Set.member "1:3" (toValidCell (getCell grid 2 3)).links) &&
            (Set.member "2:3" (toValidCell (getCell grid 3 3)).links)))
            ]

main = runDisplay sidewinderTests

