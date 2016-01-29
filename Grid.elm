module Grid where

import Mask exposing (Mask)
import Cell exposing (Cell)
import PolarCell exposing (PolarCell)

import Set
import List
import Array
import String
import Color
import Rnd exposing (..)
import Random exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Color exposing (..)
import Text exposing (..)

-- Abstract cell list type
type GridCell
    = RectCellTag Cell
    | PolarCellTag PolarCell

-- made extensible to contain additional data (ie. distances)
type alias Grid a =
    {a |
        rows: Int,
        cols: Int,
        cells: List GridCell,
        cellMaker: (Mask -> List GridCell),
        rnd: GridRnd,
        mask : Mask
}

type alias RowAscii = {
    top : String,
    bottom : String
}

-- constructor
--createGrid : Int -> Int -> Seed -> Grid a
createGrid rows cols initSeed cellMaker =
    let mask' = Mask.createMask rows cols
    in {
           rows = rows,
           cols = cols,
           -- loop rows times
           cells = cellMaker mask',
           rnd = createGridRnd rows cols initSeed,
           mask = mask'
       }

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
makeCells : Mask -> List GridCell
makeCells mask =
    let createMaskedCell row col =
        if Mask.get mask row col
           then RectCellTag (Cell.createCell row col)
           else RectCellTag (Cell.createMaskedCell row col)

        makeRow cols row =
            List.map (createMaskedCell row) [1..(mask.cols)]
    in
       List.concatMap (makeRow mask.cols) [1..(mask.rows)]

-- generates collage object (Element) of the grid
-- Takes 2 painter functions: one for the whole grid and one for each cell
toElement grid gridPainter cellPainter cellSize =
    gridPainter cellPainter grid cellSize

-- Returns string ASCII representation of a grid
toAscii : Grid a -> (Grid a -> Cell -> String) -> String
toAscii grid cellViewer =
    let cellToString : Cell -> RowAscii -> RowAscii
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
                finalascii = List.foldl cellToString rowascii (gridRowCells grid row)
            in
               finalascii.top ++ "\n" ++ finalascii.bottom ++ "\n"
    in
       "+" ++ (String.repeat grid.cols "---+") ++ "\n" ++
       String.concat (List.map rowToStrings [1..grid.rows])

-- generates rectangular grid element
painter : (Grid a -> Cell -> Color) -> Grid a -> Int -> Element
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

        cellWalls : LineStyle -> Cell -> List Form
        cellWalls style cell =
            let x1 = toFloat ((cell.col - 1) * cellSize)
                y1 = toFloat (negate (cell.row - 1) * cellSize)
                x2 = toFloat (cell.col * cellSize)
                y2 = toFloat (negate cell.row  * cellSize)
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

        cellBackground : LineStyle -> Cell -> Form
        cellBackground style cell =
            let bgRect = filled (cellPainter grid cell) (cellToRect cell)
                --dbg = outlinedText style (Text.fromString " C ")
                halfSize = (toFloat cellSize) / 2.0
                cx = toFloat ((cell.col - 1) * cellSize) + halfSize
                cy = toFloat (negate (cell.row - 1) * cellSize) - halfSize
            in
               move (cx, cy) bgRect

        cellToRect : Cell -> Shape
        cellToRect cell =
            square <| toFloat cellSize

        paintCell : Cell -> Form
        paintCell cell =
            let style = { defaultLine | width = 2 }
            in
               group <| ((cellBackground style cell) :: (cellWalls style cell))

        -- cast grid.cells to rectCells
        drawables = List.map paintCell (List.map toRectCell grid.cells)
    in
       collage imgWidth imgHeight [group drawables |> move (ox, oy)]

-- returns cell at an x,y index.
-- returns nil cell if the index is invalid or the cell at that location is masked
getCell : Grid a -> Int -> Int -> Maybe GridCell
getCell grid row col =
    -- validate bounds
    if (row > grid.rows || col > grid.cols || row <= 0 || col <= 0)
       then Nothing
       else
       let cell = Array.get ((gridIndex grid row col) - 1) <| Array.fromList grid.cells
       in
          case cell of
              Just (RectCellTag c) ->
                  if c.masked
                     then Nothing
                     else Just (RectCellTag c)
              Just (PolarCellTag c) ->
                  if c.masked
                     then Nothing
                     else Just (PolarCellTag c)
              Nothing -> Nothing

-- commonly used to map a maybe cell to a cell
toValidCell : Maybe Cell -> Cell
toValidCell cell =
    case cell of
        Just cell -> cell
        Nothing -> (Cell.createCell -1 -1)

toRectCell : GridCell -> Cell
toRectCell (RectCellTag cell) = cell

-- all this shit is bullshit
maybeGridCellToCell : Maybe GridCell -> Cell
maybeGridCellToCell cell =
    case cell of
        Nothing -> (Cell.createCell -1 -1)
        Just (RectCellTag c) -> toRectCell (RectCellTag c)

maybeGridCellToMaybeCell : Maybe GridCell -> Maybe Cell
maybeGridCellToMaybeCell cell =
    Maybe.map toRectCell cell

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
        -- TOOD: Implement for PolarCelTag
        _ -> []

-- returns all cells with only 1 link
--deadEnds : Grid a -> List GridCell
deadEnds grid =
    List.filter (\c -> (List.length (Set.toList c.links)) == 1) grid.cells

-- sometimes useful to filter the neighbors of a cell by some criteria
filterNeighbors : (GridCell -> Bool) -> Grid a -> GridCell -> List GridCell
filterNeighbors pred grid cell =
    List.filter pred <| neighbors grid cell

-- WOW THANKS TAGGED UNIONS!! NO CODE DUPLICATION HERE AT ALL NO SIRREEE
linkRectCells : Grid a -> Cell -> Cell -> Bool -> Grid a
linkRectCells grid cell cellToLink bidi =
    let linkCell : Cell -> Cell -> Cell
        linkCell cell1 cell2 = {
            cell1 | links = Set.insert cell2.id cell1.links
        }
        linkMatched : Cell -> Cell
        linkMatched c =
            if c.id == cell.id
               then linkCell c cellToLink
               else if bidi && (c.id == cellToLink.id)
                       then linkCell c cell
                       else c
    in
       {grid | cells = List.map linkMatched grid.cells}

linkPolarCells : Grid a -> PolarCell -> PolarCell -> Bool -> Grid a
linkPolarCells grid cell cellToLink bidi =
    let linkCell : PolarCell -> PolarCell -> PolarCell
        linkCell cell1 cell2 = {
            cell1 | links = Set.insert cell2.id cell1.links
        }
        linkMatched : PolarCell -> PolarCell
        linkMatched c =
            if c.id == cell.id
               then linkCell c cellToLink
               else if bidi && (c.id == cellToLink.id)
                       then linkCell c cell
                       else c
    in
       {grid | cells = List.map linkMatched grid.cells}

-- link 2 cells
linkCells : Grid a -> GridCell -> GridCell -> Bool -> Grid a
linkCells grid cell cellToLink bidi =
    case cell of
        RectCellTag c ->
            linkRectCells grid c (RectCellTag cellToLink) bidi
        PolarCellTag c ->
            linkPolarCells grid c (PolarCellTag cellToLink) bidi

-- returns all cells linked to a cell
linkedCells : Grid a -> GridCell -> List GridCell
linkedCells grid cell =
    case cell of
        RectCellTag c -> 
            List.map (cellIdToCell grid) (Set.toList c.links)
        PolarCellTag c ->
            List.map (cellIdToCell grid) (Set.toList c.links)

gridRowCells : Grid a -> Int -> List Cell
gridRowCells grid row =
    List.filter (\c -> c.row == row) grid.cells

rowCells : Grid a -> Int -> List GridCell
rowCells grid row =
       List.filter (\c -> c.row == row) grid.cells

size : Grid a -> Int
size grid =
    Mask.count grid.mask

-- cardinal index of a cell in a grid (1,1) = 1, etc
cellIndex : Grid a -> GridCell -> Int
cellIndex grid cell =
    (grid.cols * (cell.row - 1)) + cell.col

-- cardinal index of row col in a grid (1,1) = 1, etc
gridIndex : Grid a -> Int -> Int -> Int
gridIndex grid row col =
    grid.cols * (row - 1) + col

-- returns cell by its id
cellIdToCell : Grid a -> Cell.CellID -> GridCell
cellIdToCell grid cellid =
    let row = (fst cellid)
        col = (snd cellid)
        cell = getCell grid row col
    in
       case cell of
           Nothing -> RectCellTag Cell.createNilCell
           Just (RectCellTag c) -> RectCellTag c
           Just (PolarCellTag d) -> PolarCellTag d

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


