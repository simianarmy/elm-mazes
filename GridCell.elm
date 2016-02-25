module GridCell where

import Cell exposing (BaseCell, CellID, CellLinks)
import Set

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

cellToPolarCell : BaseCell -> GridCell
cellToPolarCell base =
    PolarCellTag (base, ((-1, -1), Set.empty))

toRectCell : GridCell -> BaseCell
toRectCell cell =
    case cell of
        RectCellTag c -> c
        PolarCellTag (c, _) -> c

toPolarCell : GridCell -> (BaseCell, (CellID, CellLinks))
toPolarCell cell =
    case cell of
        RectCellTag c -> (c, ((-1, -1), Set.empty))
        PolarCellTag c -> c

setInwardCell : GridCell -> GridCell -> GridCell
setInwardCell cell inward =
    let (c, (cid, links)) = toPolarCell cell
        (ic, (icid, _)) = toPolarCell inward
    in
       PolarCellTag (c, (icid, links))

addOutwardLink : GridCell -> GridCell -> GridCell
addOutwardLink parentCell outwardCell =
    let (pcell, (pcid, pclinks)) = toPolarCell parentCell
        (cell, (cid, clinks)) = toPolarCell outwardCell
        newLinks = Set.insert cell.id pclinks
    in
       PolarCellTag (pcell, (pcid, newLinks))

