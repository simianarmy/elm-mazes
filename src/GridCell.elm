module GridCell exposing (..)

import Arithmetic
import Cell exposing (BaseCell, CellID, CellLinks)
import Set



-- RG says a better way might be to extract common properties into a new type and add the differences to the tag function like so


type GridCell
    = RectCellTag BaseCell
    | PolarCellTag ( BaseCell, ( CellID, CellLinks ) )
    | HexCellTag BaseCell
    | TriangleCellTag BaseCell
    | OuterCellTag BaseCell



-- attribute functions


id : GridCell -> CellID
id gc =
    case gc of
        PolarCellTag ( bc, _ ) ->
            bc.id

        _ ->
            .id <| base gc


row : GridCell -> Int
row gc =
    Tuple.first <| id gc


col : GridCell -> Int
col gc =
    Tuple.second <| id gc


isValidCell : Maybe GridCell -> Bool
isValidCell cell =
    case cell of
        Nothing ->
            False

        Just cell ->
            True



-- returns cell's base object (destructures union type)


base : GridCell -> BaseCell
base gc =
    case gc of
        RectCellTag b ->
            b

        PolarCellTag ( b, _ ) ->
            b

        HexCellTag b ->
            b

        TriangleCellTag b ->
            b

        OuterCellTag b ->
            b


links : GridCell -> CellLinks
links gc =
    .links <| base gc


cellToPolarCell : BaseCell -> GridCell
cellToPolarCell base =
    PolarCellTag ( base, ( ( -1, -1 ), Set.empty ) )



-- alias for base


toRectCell : GridCell -> BaseCell
toRectCell cell =
    base cell


toPolarCell : GridCell -> ( BaseCell, ( CellID, CellLinks ) )
toPolarCell cell =
    case cell of
        PolarCellTag c ->
            c

        _ ->
            ( base cell, ( ( -1, -1 ), Set.empty ) )


setInwardCell : GridCell -> GridCell -> GridCell
setInwardCell cell inward =
    let
        ( c, ( cid, links ) ) =
            toPolarCell cell

        ( ic, ( icid, _ ) ) =
            toPolarCell inward
    in
    PolarCellTag ( c, ( ic.id, links ) )


addOutwardLink : GridCell -> GridCell -> GridCell
addOutwardLink parentCell outwardCell =
    let
        ( pcell, ( pcid, pclinks ) ) =
            toPolarCell parentCell

        ( cell, ( cid, clinks ) ) =
            toPolarCell outwardCell

        newLinks =
            Set.insert cell.id pclinks
    in
    PolarCellTag ( pcell, ( pcid, newLinks ) )



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
    Maybe.withDefault (RectCellTag Cell.createNilCell) cell



-- Helper to apply filter to list of gridcells


filterGridCells : (BaseCell -> Bool) -> List GridCell -> List GridCell
filterGridCells fn cells =
    List.filter (fn << base) cells



-- this doesn't make sense to me anymore.  I guess it uses the type of the 1st
-- param to create a new object using the 2nd


cellToGridCell : GridCell -> BaseCell -> GridCell
cellToGridCell gc bc =
    case gc of
        RectCellTag c ->
            RectCellTag bc

        PolarCellTag ( p, rest ) ->
            PolarCellTag ( bc, rest )

        HexCellTag c ->
            HexCellTag bc

        TriangleCellTag c ->
            TriangleCellTag bc

        OuterCellTag c ->
            OuterCellTag bc


setVisited : GridCell -> GridCell
setVisited gc =
    let
        bc =
            base gc

        bc_ =
            { bc | visited = True }
    in
    cellToGridCell gc bc_



-- probably a better way to update a base property...


setTag : GridCell -> String -> GridCell
setTag gc strTag =
    let
        bc =
            base gc

        bc_ =
            { bc | tag = strTag }
    in
    cellToGridCell gc bc_



-- probably a better way to update a base property...


setWeight : GridCell -> Int -> GridCell
setWeight gc w =
    let
        bc =
            base gc

        bc_ =
            { bc | weight = w }
    in
    cellToGridCell gc bc_


toString : GridCell -> String
toString gc =
    Cell.cellToString (base gc)
