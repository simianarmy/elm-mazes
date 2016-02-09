module GridCell where

import Cell exposing (BaseCell, CellID, CellLinks)

-- Abstract cell list type
-- type OldGridCell
--     = RectCellTag Cell
--     | PolarCellTag PolarCell

-- RG says a better way might be to extract common properties into a new type and add the differences to the tag function like so
type GridCell
    = RectCellTag BaseCell
    | PolarCellTag (BaseCell, (CellID, CellLinks))

id : GridCell -> CellID
id gc =
    case gc of
        RectCellTag bc -> bc.id
        PolarCellTag (bc, _) -> bc.id

