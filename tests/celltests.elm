-- cellTests.elm


module Main exposing (cellTests, createGridCell, main, testLinkCells)

import Cell exposing (..)
import Element exposing (Element)
import ElmTest exposing (..)
import Grid exposing (..)
import GridCell exposing (..)
import Set exposing (..)
import TestHelpers exposing (..)


createGridCell row col =
    Cell.createCell row col |> GridCell.RectCellTag


testLinkCells : Bool -> ( Cell, Cell )
testLinkCells bidi =
    let
        cell1 =
            createGridCell 1 1

        cell2 =
            createGridCell 2 2

        grid =
            linkCells (TestHelpers.createGrid 3 3) cell1 cell2 bidi
    in
    ( maybeGridCellToCell (getCell grid 1 1), maybeGridCellToCell (getCell grid 2 2) )


cellTests : Test
cellTests =
    suite "Cell test suite"
        [ test "Generating ID" (assertEqual (createCellID 1 2) ( 1, 2 ))
        , test "Cell creator" (assertEqual (createCell 1 2).id ( 1, 2 ))
        , test "Cell row coordinates" (assertEqual (createCell 1 2).row 1)
        , test "Cell col coordinates" (assertEqual (createCell 1 2).col 2)

        -- test links between cells
        , test "Cells link data unidirectional" (assert (Set.member ( 2, 2 ) (Tuple.first (testLinkCells False)).links))
        , test "Cells link data unidirectional2" (assertEqual (Set.member ( 1, 1 ) (Tuple.second (testLinkCells False)).links) False)
        , test "Cells link data bidirectional"
            (let
                lcells =
                    testLinkCells True
             in
             assert (Set.member ( 2, 2 ) (Tuple.first lcells).links && Set.member ( 1, 1 ) (Tuple.second lcells).links)
            )
        , test "Cell linked query"
            (assert
                (let
                    cells =
                        testLinkCells False
                 in
                 isLinked (Tuple.first cells) (Tuple.second cells)
                )
            )
        , test "Cell linked query unidirectional"
            (assertEqual
                (let
                    cells =
                        testLinkCells False
                 in
                 isLinked (Tuple.second cells) (Tuple.first cells)
                )
                False
            )
        , test "Cell linked query bidirectional"
            (assert
                (let
                    cells =
                        testLinkCells True
                 in
                 isLinked (Tuple.second cells) (Tuple.first cells)
                )
            )
        , test "Linking cells preserves cell properties"
            (let
                cell1 =
                    createGridCell 1 1

                cell1Processed =
                    setProcessed cell1

                cell2 =
                    createGridCell 2 2

                grid =
                    Debug.log "grid" <| linkCells (TestHelpers.createGrid 3 3) cell1Processed cell2 True
             in
             assert (maybeGridCellToCell <| getCell grid 1 1).processed
            )
        ]


main =
    elementRunner cellTests
