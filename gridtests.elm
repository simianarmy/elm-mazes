-- gridTests.elm
import Grid exposing (..)
import Cell exposing (..)
import Set

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)
import Html exposing (..)
import Random exposing (..)

-- helps with getCell
unmaybeCell cell =
    (cell |> Maybe.map (Cell.cellToString) |> Maybe.withDefault "")

gridTests : Test
gridTests = suite "Grid test suite"
        [ test "Grid creator" (assertEqual (List.length (createGrid 2 2).cells) 4)
        , test "Cell accessor" (assertEqual (unmaybeCell (getCell (createGrid 2 2) 1 2)) "(1, 2)")
        , test "Cell accessor2" (assertEqual (unmaybeCell (getCell (createGrid 2 3) 1 2)) "(1, 2)")
        , test "Cell accessor3" (assertEqual (unmaybeCell (getCell (createGrid 2 3) 2 1)) "(2, 1)")
        , test "Cell accessor out of bounds" 
            (assertEqual (unmaybeCell (getCell (createGrid 2 3) 3 4)) "")
        , test "Cell accessor out of bounds2" 
            (assertEqual (unmaybeCell (getCell (createGrid 2 3) 0 4)) "")
        , test "Neighbors returns list of neighboring cells (middle of 3x3 grid)" (
            let grid = createGrid 3 3
            in
                assertEqual (List.length (neighbors grid (createCell 2 2))) 4)
        , test "Neighbors returns list of neighboring cells (corner of 3x3 grid)" (
            let grid = createGrid 3 3
            in
                assertEqual (List.length (neighbors grid (createCell 1 1))) 2)
        , test "Neighbors returns list of neighboring cells (corner 2x2 grid)" (
            let grid = createGrid 2 2
            in
                assertEqual (List.length (neighbors grid (createCell 1 1))) 2)
        , test "Neighbors returns list of neighboring cells (corner 1x2 grid)" (
            let grid = createGrid 1 2
            in
                assertEqual (List.length (neighbors grid (createCell 1 1))) 1)
        , test "Neighbors returns list of neighboring cells (corner 1x1 grid)" (
            let grid = createGrid 1 1
            in
                assertEqual (List.length (neighbors grid (createCell 1 1))) 0)
        , test "Grid size" (assertEqual (Grid.size (createGrid 3 5)) 15)
        ]

main = runDisplay gridTests
