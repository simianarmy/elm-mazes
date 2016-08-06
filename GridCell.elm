module GridCell where

import Cell exposing (BaseCell, CellID, CellLinks)
import Set
import Arithmetic

-- RG says a better way might be to extract common properties into a new type and add the differences to the tag function like so
type GridCell
    = RectCellTag BaseCell
    | PolarCellTag (BaseCell, (CellID, CellLinks))
    | HexCellTag BaseCell
    | TriangleCellTag BaseCell

-- attribute functions
id : GridCell -> CellID
id gc =
    case gc of
        RectCellTag bc -> bc.id
        PolarCellTag (bc, _) -> bc.id
        HexCellTag bc -> bc.id
        TriangleCellTag bc -> bc.id

row : GridCell -> Int
row gc =
    fst <| id gc

col : GridCell -> Int
col gc =
    snd <| id gc

isValidCell : Maybe GridCell -> Bool
isValidCell cell =
    case cell of
        Nothing -> False
        Just cell -> True

-- returns cell's base object
base : GridCell -> BaseCell
base gc =
    case gc of
        RectCellTag bc -> bc
        PolarCellTag (bc, _) -> bc
        HexCellTag bc -> bc
        TriangleCellTag bc -> bc

links : GridCell -> CellLinks
links gc =
    .links <| base gc

cellToPolarCell : BaseCell -> GridCell
cellToPolarCell base =
    PolarCellTag (base, ((-1, -1), Set.empty))

-- alias for base
toRectCell : GridCell -> BaseCell
toRectCell cell = base cell

toPolarCell : GridCell -> (BaseCell, (CellID, CellLinks))
toPolarCell cell =
    case cell of
        PolarCellTag c -> c
        RectCellTag c -> (c, ((-1, -1), Set.empty))
        HexCellTag c -> (c, ((-1, -1), Set.empty))
        TriangleCellTag c -> (c, ((-1, -1), Set.empty))

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

-- helpful type functions
maybeGridCellToCell : Maybe GridCell -> BaseCell
maybeGridCellToCell cell =
    maybeGridCellToGridCell cell
    |> base

maybeGridCellToMaybeCell : Maybe GridCell -> Maybe BaseCell
maybeGridCellToMaybeCell cell =
    Maybe.map base cell

-- defaults to nil RectCellTag
maybeGridCellToGridCell : Maybe GridCell -> GridCell
maybeGridCellToGridCell cell =
    case cell of
        Nothing -> RectCellTag Cell.createNilCell
        Just (RectCellTag c) -> RectCellTag c
        Just (PolarCellTag p) -> PolarCellTag p
        Just (HexCellTag c) -> HexCellTag c
        Just (TriangleCellTag c) -> TriangleCellTag c

-- Helper to apply filter to list of gridcells
filterGridCells : (BaseCell -> Bool) -> List GridCell -> List GridCell
filterGridCells fn cells =
    List.filter (fn << base) cells

cellToGridCell : GridCell -> BaseCell -> GridCell
cellToGridCell gc bc =
       case gc of
           RectCellTag c -> RectCellTag bc
           PolarCellTag (p, rest) -> PolarCellTag (bc, rest)
           HexCellTag c -> HexCellTag bc
           TriangleCellTag c -> TriangleCellTag bc

setVisited: GridCell -> GridCell
setVisited gc =
    let bc = base gc
        bc' = { bc | visited = True }
    in
       cellToGridCell gc bc'

-- probably a better way to update a base property...
setTag : GridCell -> String -> GridCell
setTag gc strTag =
    let bc = base gc
        bc' = { bc | tag = strTag }
    in
       cellToGridCell gc bc'

-- probably a better way to update a base property...
setWeight : GridCell -> Int -> GridCell
setWeight gc w =
    let bc = base gc
        bc' = { bc | weight = w }
    in
       cellToGridCell gc bc'

toString : GridCell -> String
toString gc = Cell.cellToString (base gc)
