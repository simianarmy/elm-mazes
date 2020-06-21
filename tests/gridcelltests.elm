module GridCellTests exposing (..)

-- gridcellTests.elm

import Cell
import Expect
import Grid exposing (..)
import GridCell exposing (..)
import Test exposing (..)
import TestHelpers exposing (..)


createRectCell row col =
    Cell.createCell row col |> GridCell.RectCellTag


all : Test
all =
    describe "GridCell test suite"
        [ test "Rect cell toString" <| \() -> Expect.equal (GridCell.toString (createRectCell 1 2)) "(1, 2)"
        , test "setTag" <|
            \() ->
                let
                    rc =
                        createRectCell 1 1

                    rcp =
                        GridCell.setTag rc "tagA"
                in
                Expect.equal "tagA" (.tag (GridCell.base rcp))
        , test "linkCells" <|
            \() ->
                let
                    grid =
                        TestHelpers.createGrid 2 2

                    c1 =
                        createRectCell 0 0

                    c2 =
                        createRectCell 0 1

                    grid_ =
                        linkCells grid c1 c2 True

                    c1_ =
                        maybeGridCellToGridCell (getCell grid_ 0 0)
                in
                Expect.true "failed" (Cell.isLinked (GridCell.base c1_) (GridCell.base c2))
        ]
