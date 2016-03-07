module GridCell where

import Cell exposing (BaseCell, CellID, CellLinks)
import Set


-- RG says a better way might be to extract common properties into a new type and add the differences to the tag function like so
type GridCell
    = RectCellTag BaseCell
    | PolarCellTag (BaseCell, (CellID, CellLinks))
    | HexCellTag BaseCell

-- attribute functions
id : GridCell -> CellID
id gc =
    case gc of
        RectCellTag bc -> bc.id
        PolarCellTag (bc, _) -> bc.id
        HexCellTag bc -> bc.id

row : GridCell -> Int
row gc =
    fst <| id gc

col : GridCell -> Int
col gc =
    snd <| id gc

-- returns cell's base object
base : GridCell -> BaseCell
base gc =
    case gc of
        RectCellTag bc -> bc
        PolarCellTag (bc, _) -> bc
        HexCellTag bc -> bc

cellToPolarCell : BaseCell -> GridCell
cellToPolarCell base =
    PolarCellTag (base, ((-1, -1), Set.empty))

toRectCell : GridCell -> BaseCell
toRectCell cell =
    case cell of
        RectCellTag c -> c
        PolarCellTag (c, _) -> c
        HexCellTag c -> c

toPolarCell : GridCell -> (BaseCell, (CellID, CellLinks))
toPolarCell cell =
    case cell of
        PolarCellTag c -> c
        RectCellTag c -> (c, ((-1, -1), Set.empty))
        HexCellTag c -> (c, ((-1, -1), Set.empty))


setInwardCell : GridCell -> GridCell -> GridCell
setInwardCell cell inward =
    let (c, (cid, links)) = toPolarCell cell
        (ic, (icid, _)) = toPolarCell inward
    in
       PolarCellTag (c, (ic.id, links))

addOutwardLink : GridCell -> GridCell -> GridCell
addOutwardLink parentCell outwardCell =
    let (pcell, (pcid, pclinks)) = toPolarCell parentCell
        (cell, (cid, clinks)) = toPolarCell outwardCell
        newLinks = Set.insert cell.id pclinks
    in
       PolarCellTag (pcell, (pcid, newLinks))

