-- cellTests.elm
import Cell exposing (..)
import Set exposing (..)

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)
import Html exposing (..)

unmaybeCell cell =
    (cell |> Maybe.map (Cell.cellToString) |> Maybe.withDefault "")

linkCells : Bool -> (Cell, Cell)
linkCells bidi =
    let cell1 = createCell 1 1
        cell2 = createCell 2 2
    in
       linkCell cell1 cell2 bidi

unlinkCells : Bool -> (Cell, Cell)
unlinkCells bidi =
    let linked = linkCells bidi
    in
       unlinkCell (fst linked) (snd linked) bidi 

cellTests : Test
cellTests = suite "Cell test suite"
        [ test "Generating ID" (assertEqual (createCellID 1 2) "1:2")
        , test "Cell creator" (assertEqual (createCell 1 2).id "1:2")
        , test "Cell coordinates" (assertEqual (createCell 1 2).row 1)
        -- test links between cells
        , test "Cells link data unidirectional" (assert (Set.member "2:2" (fst (linkCells False)).links))
        , test "Cells link data unidirectional2" (assertEqual (Set.member "1:1" (snd (linkCells False)).links) False)
        , test "Cells link data bidirectional" (assert (Set.member "2:2" (fst (linkCells True)).links))
        , test "Cells link data bidirectional2" (assert (Set.member "1:1" (snd (linkCells True)).links))
        -- test unlinking
        , test "Cells unlink data unidirectional" (assertEqual (Set.member "2:2" (fst (unlinkCells False)).links) False)
        , test "Cells unlink data unidirectional2" (assertEqual (Set.member "1:1" (snd (unlinkCells False)).links) False)
        , test "Cells unlink data bidirectional" (assertEqual (Set.member "2:2" (fst (unlinkCells True)).links) False)
        , test "Cells unlink data bidirectional2" (assertEqual (Set.member "1:1" (snd (unlinkCells True)).links) False)
        , test "Cell linked query" (assert (let cells = linkCells False
                                            in isLinked (fst cells) (snd cells)))
        , test "Cell linked query unidirectional" (assertEqual (let cells = linkCells False
                                            in isLinked (snd cells) (fst cells)) False)
        , test "Cell linked query bidirectional" (assert (let cells = linkCells True
                                            in isLinked (snd cells) (fst cells)))
        , test "Cell linked query when unlinked" (assertEqual (
            let cells = unlinkCells True
            in isLinked (fst cells) (snd cells)) False)
        ]

main = runDisplay cellTests
