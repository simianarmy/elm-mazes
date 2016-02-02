module GridCell where

import Cell exposing (BaseCell, CellLinks)

-- Abstract cell list type
-- type OldGridCell
--     = RectCellTag Cell
--     | PolarCellTag PolarCell

-- RG says a better way might be to extract common properties into a new type and add the differences to the tag function like so
type GridCell
    = RectCellTag BaseCell
    | PolarCellTag BaseCell CellLinks

