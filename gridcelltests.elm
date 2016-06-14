-- gridcellTests.elm
import TestHelpers exposing (..)
import GridCell exposing (..)
import Grid exposing (..)
import Cell

import ElmTest exposing (..)
import Graphics.Element exposing (Element)

createRectCell row col =
    Cell.createCell row col |> GridCell.RectCellTag

gridcellTests : Test
gridcellTests = suite "GridCell test suite"
    [
        test "Rect cell toString" (assertEqual (GridCell.toString (createRectCell 1 2)) "(1, 2)")
        , test "setProcessed" (
            let rc = (createRectCell 1 1)
                rcp = setProcessed rc
            in
               assert ((not (base rc).processed) && (base rcp).processed)
           )
        , test "setProcessed maintains links" (
            let grid = TestHelpers.createGrid 2 2
                c1 = createRectCell 0 0
                c2 = createRectCell 0 1
                grid' = linkCells grid c1 c2 True
                c1' = maybeGridCellToGridCell (getCell grid' 0 0)
                pc1 = Debug.log "pc1" <| setProcessed c1'
            in
               assert (Cell.isLinked (GridCell.base pc1) (GridCell.base c2))
           )
        ]

main = elementRunner gridcellTests
