-- gridcellTests.elm
import TestHelpers exposing (..)
import GridCell exposing (..)
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
        ]

main = elementRunner gridcellTests
