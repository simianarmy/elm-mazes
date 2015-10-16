-- cellTests.elm
import Cell exposing (..)
import Set exposing (..)
import Grid exposing (..)

import Random
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)
import Html exposing (..)

unmaybeCell cell =
    (cell |> Maybe.map (Cell.cellToString) |> Maybe.withDefault "")

testLinkCells : Bool -> (Cell, Cell)
testLinkCells bidi =
    let cell1 = createCell 1 1
        cell2 = createCell 2 2
        seed = Random.initialSeed 123
        grid = linkCells (createGrid 2 2 seed) cell1 cell2 bidi
    in
       (toValidCell (getCell grid 1 1), toValidCell (getCell grid 2 2))

testUnlinkCells : Bool -> (Cell, Cell)
testUnlinkCells bidi =
    let linked = testLinkCells bidi
        seed = Random.initialSeed 123
        grid = unlinkCells (createGrid 2 2 seed) (fst linked) (snd linked) bidi
    in
       (toValidCell (getCell grid 1 1), toValidCell (getCell grid 2 2))

cellTests : Test
cellTests = suite "Cell test suite"
        [ test "Generating ID" (assertEqual (createCellID 1 2) "1:2")
        , test "Cell creator" (assertEqual (createCell 1 2).id "1:2")
        , test "Cell coordinates" (assertEqual (createCell 1 2).row 1)
        -- test links between cells
        , test "Cells link data unidirectional" (assert (Set.member "2:2" (fst (testLinkCells False)).links))
        , test "Cells link data unidirectional2" (assertEqual (Set.member "1:1" (snd (testLinkCells False)).links) False)
        , test "Cells link data bidirectional" (
            let lcells = testLinkCells True
            in
                assert ((Set.member "2:2" (fst lcells).links) && (Set.member "1:1" (snd lcells).links)))
        -- test unlinking
        , test "Cells unlink data unidirectional" (assertEqual (Set.member "2:2" (fst (testUnlinkCells False)).links) False)
        , test "Cells unlink data unidirectional2" (assertEqual (Set.member "1:1" (snd (testUnlinkCells False)).links) False)
        , test "Cells unlink data bidirectional" (assertEqual (Set.member "2:2" (fst (testUnlinkCells True)).links) False)
        , test "Cells unlink data bidirectional2" (assertEqual (Set.member "1:1" (snd (testUnlinkCells True)).links) False)
        , test "Cell linked query" (assert (let cells = testLinkCells False
                                            in isLinked (fst cells) (snd cells)))
        , test "Cell linked query unidirectional" (assertEqual (let cells = testLinkCells False
                                            in isLinked (snd cells) (fst cells)) False)
        , test "Cell linked query bidirectional" (assert (let cells = testLinkCells True
                                            in isLinked (snd cells) (fst cells)))
        , test "Cell linked query when unlinked" (assertEqual (
            let cells = testUnlinkCells True
            in isLinked (fst cells) (snd cells)) False)
        ]

main = runDisplay cellTests
