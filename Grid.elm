module Grid where

import Mask exposing (Mask)
import Cell exposing (BaseCell, Cell, CellID, CellLinks)
import GridCell exposing (..)

import Set exposing (Set)
import List
import Array exposing (Array)
import String
import Color
import Rnd exposing (..)
import Random exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Color exposing (..)
import Text exposing (..)

type alias CellGrid = Array (Array GridCell)

-- made extensible to contain additional data (ie. distances)
type alias Grid a =
    {a |
        rows: Int,
        cols: Int,
        cells: CellGrid,
        cellMaker: (Mask -> CellGrid),
        rnd: GridRnd,
        mask : Mask
}

type alias RowAscii = {
    top : String,
    bottom : String
}

-- constructor
-- createGrid : Int -> Int -> Seed -> (Mask -> CellGrid) -> Grid a
createGrid rows cols initSeed cellMaker =
    let mask' = Mask.createMask rows cols
    in createGridFromMask mask' initSeed cellMaker

--createGridFromMask : Mask -> Seed -> Grid a
createGridFromMask mask initSeed cellMaker =
    {
        rows = mask.rows,
        cols = mask.cols,
        cells = cellMaker mask,
        cellMaker = cellMaker,
        rnd = createGridRnd mask.rows mask.cols initSeed,
        mask = mask,
        -- WELP, NOW I HAVE TO ADD OTHER TYPES' PROPS :(
        maximum = 0,
        dists = []
    }

-- updates all rngs with fresh seeds
updateRnd : Grid a -> Grid a
updateRnd grid =
    {grid |
        rnd = Rnd.refresh grid.rnd
    }

-- regenerates all cells based the current mask
update grid =
    {grid |
        cells = grid.cellMaker grid.mask
    }

-- cells constructor for "basic" grids
makeCells : Mask -> CellGrid
makeCells mask =
    let createMaskedCell row col =
        if Mask.get mask row col
           then RectCellTag (Cell.createCell row col)
           else RectCellTag (Cell.createMaskedCell row col)

        makeRow row cols =
            Array.initialize mask.cols (\n -> createMaskedCell row n)
    in
       Array.initialize mask.rows (\n -> makeRow n mask.cols)

-- generates collage object (Element) of the grid
-- Takes 2 painter functions: one for the whole grid and one for each cell
toElement : Grid a -> 
    -- grid painter
    ((Grid a -> GridCell -> Color) -> Grid a -> Int -> Element) -> 
    -- cell painter
    (Grid a -> GridCell -> Color) -> 
    -- cell size
    Int ->
    -- returns
    Element
toElement grid gridPainter cellPainter cellSize =
    gridPainter cellPainter grid cellSize

-- Returns string ASCII representation of a grid
toAscii : Grid a -> (Grid a -> Cell -> String) -> String
toAscii grid cellViewer =
    let cellToString : BaseCell -> RowAscii -> RowAscii
        cellToString cell ascii =
            let body = " " ++ (cellViewer grid cell) ++ " "
                east_boundary = (if Cell.isLinked cell (toValidCell (east grid cell)) then " " else "|")
                south_boundary = (if Cell.isLinked cell (toValidCell (south grid cell)) then "   " else "---")
                curtop = ascii.top
                curbottom = ascii.bottom
            in
               {
                   ascii |
                   top = curtop ++ body ++ east_boundary,
                   bottom = curbottom ++ south_boundary ++ "+"
               }

        rowToStrings : Int -> String
        rowToStrings row =
            let rowascii = {
                top = "|",
                bottom = "+"
                }
                baseCells = gridCellsToBaseCells <| rowCells grid row
                finalascii = List.foldl cellToString rowascii baseCells
            in
               finalascii.top ++ "\n" ++ finalascii.bottom ++ "\n"
    in
       "+" ++ (String.repeat grid.cols "---+") ++ "\n" ++
       String.concat (List.map rowToStrings [0..grid.rows-1])

-- generates rectangular grid element
painter : (Grid a -> GridCell -> Color) -> Grid a -> Int -> Element
painter cellPainter grid cellSize =
    let imgWidth = cellSize * grid.cols
        imgHeight = cellSize * grid.rows
        ox = toFloat (negate imgWidth) / 2.0
        oy = toFloat imgHeight / 2.0

        maybeVisibleLine : LineStyle -> (Bool, Path) -> List Form
        maybeVisibleLine style (visible, seg) =
            if visible
               then [traced style seg]
               else []

        cellWalls : LineStyle -> GridCell -> List Form
        cellWalls style gridcell =
            let cell = GridCell.toRectCell gridcell
                x1 = toFloat (cell.col * cellSize)
                y1 = toFloat (negate cell.row * cellSize)
                x2 = toFloat ((cell.col + 1) * cellSize)
                y2 = toFloat (negate (cell.row + 1)  * cellSize)
            in
               if cell.masked
                  then []
                  else
                  List.concatMap (maybeVisibleLine style)
                  [
                      ((not <| isValidCell (north grid cell)), (segment (x1, y1) (x2, y1))),
                      ((not <| isValidCell (west grid cell)), (segment (x1, y1) (x1, y2))),
                      ((not <| Cell.isLinked cell (toValidCell (east grid cell))), (segment (x2, y1) (x2, y2))),
                      ((not <| Cell.isLinked cell (toValidCell (south grid cell))), (segment (x1, y2) (x2, y2)))
                      ]

        cellBackground : LineStyle -> GridCell -> Form
        cellBackground style cell =
            let rectcell = GridCell.toRectCell cell
                bgRect = filled (cellPainter grid cell) (cellToRect rectcell)
                --dbg = outlinedText style (Text.fromString " C ")
                halfSize = (toFloat cellSize) / 2.0
                cx = toFloat (rectcell.col * cellSize) + halfSize
                cy = toFloat (negate rectcell.row * cellSize) - halfSize
            in
               move (cx, cy) bgRect

        cellToRect : Cell -> Shape
        cellToRect cell =
            square <| toFloat cellSize

        paintCell : GridCell -> Form
        paintCell cell =
            let style = { defaultLine | width = 2 }
            in
               group <| ((cellBackground style cell) :: (cellWalls style cell))

        -- cast grid.cells to rectCells
        drawables = List.map paintCell (cellsList grid.cells)
    in
       collage imgWidth imgHeight [group drawables |> move (ox, oy)]

-- GRID DATA STRUCTURE CONVERSION OPERATORS
-- 1D list -> 2D array
-- 2D array -> 1D list

-- flattens 2-d array of gridcells to 1-d list
cellsList : CellGrid -> List GridCell
cellsList cells =
    List.concat <| Array.toList <| Array.map Array.toList cells

-- converts 1-d list of gridcells to 2-d array
cellsListToCellGrid : List GridCell -> CellGrid
cellsListToCellGrid cells =
    -- Determine row count
    let rowMax = Maybe.withDefault 0 <| List.maximum <| List.map (\c -> (toRectCell c).row) cells
    in
       Array.initialize (rowMax + 1) (\row -> Array.fromList <| List.filter (\c -> (toRectCell c).row == row) cells)

-- 0-based indices
-- returns cell at an x,y index.
-- returns nil cell if the index is invalid or the cell at that location is masked
getCell : {a | cells : CellGrid, rows : Int, cols : Int } 
    -> Int -> Int 
    -> Maybe GridCell
getCell grid row col =
    -- validate bounds
    -- Ignore columns to accomodate polar grids
    if (row >= grid.rows || row < 0 || col < 0)
       then Nothing
       else
       let rowCells = Maybe.withDefault Array.empty <| Array.get row grid.cells
           cell = Array.get col rowCells
       in
          case cell of
              Just (RectCellTag c) ->
                  if c.masked
                     then Nothing
                     else Just (RectCellTag c)
              Just (PolarCellTag (c, o)) ->
                  if c.masked
                     then Nothing
                     else Just (PolarCellTag (c, o))
              Nothing -> Nothing

-- commonly used to map a maybe cell to a cell
toValidCell : Maybe Cell -> Cell
toValidCell cell =
    case cell of
        Just cell -> cell
        Nothing -> Cell.createNilCell

maybeGridCellToCell : Maybe GridCell -> BaseCell
maybeGridCellToCell cell =
    case cell of
        Nothing -> Cell.createNilCell
        Just (RectCellTag c) -> c
        Just (PolarCellTag (c, _)) -> c

maybeGridCellToMaybeCell : Maybe GridCell -> Maybe Cell
maybeGridCellToMaybeCell cell =
    Maybe.map GridCell.toRectCell cell

-- defaults to RectCellTag
maybeGridCellToGridCell : Maybe GridCell -> GridCell
maybeGridCellToGridCell cell =
    case cell of
        Nothing -> RectCellTag Cell.createNilCell
        Just (RectCellTag c) -> RectCellTag c
        Just (PolarCellTag p) -> PolarCellTag p

isValidCell : Maybe Cell -> Bool
isValidCell cell =
    case cell of
        Nothing -> False
        Just cell -> True

north : Grid a -> Cell -> Maybe Cell
north grid cell =
    maybeGridCellToMaybeCell <| getCell grid (cell.row - 1) cell.col

south : Grid a -> Cell -> Maybe Cell
south grid cell =
    maybeGridCellToMaybeCell <| getCell grid (cell.row + 1) cell.col

west : Grid a -> Cell -> Maybe Cell
west grid cell =
    maybeGridCellToMaybeCell <| getCell grid cell.row (cell.col - 1)

east : Grid a -> Cell -> Maybe Cell
east grid cell =
    maybeGridCellToMaybeCell <| getCell grid cell.row (cell.col + 1)

center : Grid a -> Cell
center grid =
    maybeGridCellToCell <| getCell grid (grid.rows // 2) (grid.cols // 2)

randomCell : Grid a -> Maybe GridCell
randomCell grid =
    let (row, col) = Mask.randomLocation grid.mask grid.rnd
    in
       getCell grid row col

neighbors : Grid a -> GridCell -> List GridCell
neighbors grid cell =
    case cell of
        RectCellTag c ->
            let n = north grid c
                s = south grid c
                w = west grid c
                e = east grid c
                res = List.concat [(cellToList n), (cellToList s), (cellToList w), (cellToList e)]
            in
                List.map (\e -> RectCellTag e) res
        _ -> []

-- returns all cells with only 1 link
--deadEnds : Grid a -> List GridCell
deadEnds grid =
    List.filter (\c -> (List.length (Set.toList c.links)) == 1) (gridCellsToBaseCells (cellsList grid.cells))

-- sometimes useful to filter the neighbors of a cell by some criteria
filterNeighbors : (GridCell -> Bool) -> Grid a -> GridCell -> List GridCell
filterNeighbors pred grid cell =
    List.filter pred <| neighbors grid cell

gridCellID : GridCell -> CellID
gridCellID gc =
    case gc of
        RectCellTag c -> c.id
        PolarCellTag (c, _) -> c.id

linkCellsHelper : Grid a -> BaseCell -> CellID -> Bool -> Grid a
linkCellsHelper grid cell cellToLinkId bidi =
    let linkCell : BaseCell -> CellID -> BaseCell
        linkCell cell1 id = {
            cell1 | links = Set.insert id cell1.links
        }
        linker : BaseCell -> BaseCell
        linker c =
            if c.id == cell.id
               then linkCell c cellToLinkId
               else if bidi && (c.id == cellToLinkId)
                       then linkCell c cell.id
                       else c

        -- We still have the problem with GridCell vs BaseCell here
        linkMatched : GridCell -> GridCell
        linkMatched c =
            -- Do I extract BaseCell from GridCell here then convert back before returning?
            case c of
                RectCellTag rc -> RectCellTag (linker rc)
                PolarCellTag (pc, data) -> PolarCellTag ((linker pc), data)

    in
       {grid | cells = cellsListToCellGrid <| List.map linkMatched (cellsList grid.cells)}

-- link 2 cells
linkCells : Grid a -> GridCell -> GridCell -> Bool -> Grid a
linkCells grid cell cell2 bidi =
    let c2Id = gridCellID cell2
    in
       case cell of
           RectCellTag c ->
               linkCellsHelper grid c c2Id bidi
           PolarCellTag (c, _) ->
               linkCellsHelper grid c c2Id bidi

-- returns all cells linked to a cell
linkedCells : Grid a -> GridCell -> List GridCell
linkedCells grid cell =
    case cell of
        RectCellTag c ->
            List.map (cellIdToCell grid) (Set.toList c.links)
        PolarCellTag (c, _)  ->
            List.map (cellIdToCell grid) (Set.toList c.links)

rowMatcher : GridCell -> Int -> Bool
rowMatcher cell row =
    case cell of
        RectCellTag c -> c.row == row
        PolarCellTag (c, _) -> c.row == row

-- takes 0-indexed row
rowCells : {a| cells : CellGrid} -> Int -> List GridCell
rowCells grid row =
    Array.toList <| Maybe.withDefault Array.empty <| Array.get row grid.cells

-- helper to pattern match list of unions
gridCellsToBaseCells : List GridCell -> List BaseCell
gridCellsToBaseCells gridcells =
    List.map GridCell.toRectCell gridcells

-- Not all grid types have the same # cells
size : Grid a -> Int
size grid =
    case maybeGridCellToGridCell <| getCell grid 0 0 of
        PolarCellTag c -> List.length <| cellsList grid.cells
        _ -> Mask.count grid.mask

-- cardinal index of a cell in a grid (1,1) = 1, etc
cellIndex : Grid a -> GridCell -> Int
cellIndex grid cell =
    let rc = GridCell.toRectCell cell
    in
       grid.cols * rc.row + rc.col

-- cardinal index of row col in a grid (0,0) = 0, etc
gridIndex : {a | cols : Int } -> Int -> Int -> Int
gridIndex grid row col =
    grid.cols * row + col

-- returns cell by its id
cellIdToCell : Grid a -> Cell.CellID -> GridCell
cellIdToCell grid cellid =
    let row = (fst cellid)
        col = (snd cellid)
    in
        maybeGridCellToGridCell <| getCell grid row col

-- Helper to make a maybe cell a list (empty if maybe)
cellToList : Maybe Cell -> List Cell
cellToList cell =
    case cell of
        Just cell -> [cell]
        Nothing -> []

toTitle : Grid a -> String
toTitle grid =
    toString grid.rows ++ " X " ++ toString grid.cols ++ " Grid"

cellToAscii : Grid a -> Cell -> String
cellToAscii grid cell = 
    if cell.masked
       then "M"
       else " "

cellBackgroundColor : Grid a -> GridCell -> Color
cellBackgroundColor grid cell =
    Color.white


