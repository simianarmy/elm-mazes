module Grid where

import Cell exposing (..)
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

-- made extensible to contain additional data (ie. distances)
type alias Grid a =
    {a | 
        rows: Int,
        cols: Int,
        cells: List Cell,
        rnd: GridRnd
}

createGrid : Int -> Int -> Seed -> Grid {}
createGrid rows cols initSeed =
    let 
        makeRow : Int -> Int -> List Cell
        makeRow cols row =
            List.map
                (Cell.createCell row)
                [1..cols]

    in
       -- loop rows times
       {
           rows = rows,
           cols = cols,
           cells = List.concatMap (makeRow cols) [1..rows],
           rnd = createGridRnd rows cols initSeed
       }

-- random number helpers
nextSeed : Grid a -> Seed
nextSeed grid =
    (refresh grid.rnd).seed

updateRnd : Grid a -> Grid a
updateRnd grid =
    {
        grid |
        rnd <- Rnd.refresh grid.rnd
    }

update : Grid a -> Grid {}
update grid =
    createGrid grid.rows grid.cols <| nextSeed grid

-- generates collage view of the grid
view : (Grid a -> Cell -> Color) -> Grid a -> Int -> Element
view cellPainter grid cellSize =
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
            let style = { defaultLine | width <- 2 }
            in
               group <| ((cellBackground style cell) :: (cellWalls style cell))

        drawables = List.map paintCell grid.cells
    in
       collage imgWidth imgHeight [group drawables |> move (ox, oy)]

getCell : Grid a -> Int -> Int -> Maybe Cell
getCell grid row col =
    if (row > grid.rows || col > grid.cols || row <= 0 || col <= 0)
       then Nothing
       else Array.get ((gridIndex grid row col) - 1) <| Array.fromList grid.cells

-- commonly used to map a maybe cell to a cell
toValidCell : Maybe Cell -> Cell
toValidCell cell =
    case cell of
        Nothing -> createCell -1 -1
        Just cell -> cell

isValidCell : Maybe Cell -> Bool
isValidCell cell =
    case cell of
        Nothing -> False
        Just cell -> True

north : Grid a -> Cell -> Maybe Cell
north grid cell =
    getCell grid (cell.row - 1) cell.col

south : Grid a -> Cell -> Maybe Cell
south grid cell =
    getCell grid (cell.row + 1) cell.col

west : Grid a -> Cell -> Maybe Cell
west grid cell =
    getCell grid cell.row (cell.col - 1)

east : Grid a -> Cell -> Maybe Cell
east grid cell =
    getCell grid cell.row (cell.col + 1)

center : Grid a -> Cell
center grid =
    toValidCell <| getCell grid (grid.rows // 2) (grid.cols // 2)

randomCell : Grid a -> (Grid a, Cell)
randomCell grid =
    let grid' = updateRnd grid
        randRow = grid'.rnd.row
        randCol = grid'.rnd.col
        cell = toValidCell <| getCell grid' randRow randCol
    in
       (grid', cell)

neighbors : Grid a -> Cell -> List Cell
neighbors grid cell =
    let n = north grid cell
        s = south grid cell
        w = west grid cell
        e = east grid cell
    in 
       List.concat [(cellToList n), (cellToList s), (cellToList w), (cellToList e)]

-- sometimes useful to filter the neighbors of a cell by some criteria
filterNeighbors : (Cell -> Bool) -> Grid a -> Cell -> List Cell
filterNeighbors pred grid cell =
    List.filter pred <| neighbors grid cell

-- link 2 cells
linkCells : Grid a -> Cell -> Cell -> Bool -> Grid a
linkCells grid cell cellToLink bidi =
    let linkCell : Cell -> Cell -> Cell
        linkCell cell1 cell2 = {
            cell1 | links <- Set.insert cell2.id cell1.links
        }
        linkMatched : Cell -> Cell
        linkMatched c =
            if c.id == cell.id
               then linkCell c cellToLink
               else if bidi && (c.id == cellToLink.id)
                       then linkCell c cell
                       else c
    in
        {grid | cells <- List.map linkMatched grid.cells}

-- unlink 2 cells w/ optional bidirectional flag
unlinkCells : Grid a -> Cell -> Cell -> Bool -> Grid a
unlinkCells grid cell cellToUnlink bidi =
    let unlinkCell : Cell -> Cell -> Cell
        unlinkCell cell1 cell2 = {
            cell1 | links <- Set.remove cell2.id cell1.links
        }
        unlinkMatched : Cell -> Cell
        unlinkMatched c =
            if c.id == cell.id
               then unlinkCell c cellToUnlink
               else if bidi
                       then unlinkCell cellToUnlink cell
                       else c
    in
       {grid | cells <- List.map unlinkMatched grid.cells}

-- returns all cells linked to a cell
linkedCells : Grid a -> Cell -> List Cell
linkedCells grid cell =
    List.map (cellIdToCell grid) (Set.toList cell.links)

rowCells : Grid a -> Int -> List Cell
rowCells grid row =
    List.filter (\c -> c.row == row) grid.cells

size : Grid a -> Int
size grid =
    grid.rows * grid.cols

-- cardinal index of a cell in a grid (1,1) = 1, etc
cellIndex : Grid a -> Cell -> Int
cellIndex grid cell =
    (grid.cols * (cell.row - 1)) + cell.col

-- cardinal index of row col in a grid (1,1) = 1, etc
gridIndex : Grid a -> Int -> Int -> Int
gridIndex grid row col =
    grid.cols * (row - 1) + col

-- returns cell by its id
cellIdToCell : Grid a -> CellID -> Cell
cellIdToCell grid cellid =
    let row = (fst cellid)
        col = (snd cellid)
    in
       toValidCell <| getCell grid row col

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
cellToAscii grid cell = " "

cellBackgroundColor : Grid a -> Cell -> Color
cellBackgroundColor grid cell =
    Color.white

-- Returns ASCII representation of a grid
type alias RowAscii = {
    top : String, 
    bottom : String
}

toAscii : (Grid a -> Cell -> String) -> Grid a -> String
toAscii cellViewer grid =
    let cellToString : Cell -> RowAscii -> RowAscii
        cellToString cell ascii =
            let body = " " ++ (cellViewer grid cell) ++ " "
                east_boundary = (if isLinked cell (toValidCell (east grid cell)) then " " else "|")
                south_boundary = (if isLinked cell (toValidCell (south grid cell)) then "   " else "---")
                curtop = ascii.top
                curbottom = ascii.bottom
            in
               {
                   ascii |
                   top <- curtop ++ body ++ east_boundary,
                   bottom <- curbottom ++ south_boundary ++ "+"
               }

        rowToStrings : Int -> String
        rowToStrings row =
            let rowascii = {
                top = "|",
                bottom = "+"
            }
                finalascii = List.foldl cellToString rowascii (rowCells grid row)
            in
               finalascii.top ++ "\n" ++ finalascii.bottom ++ "\n"
    in
       "+" ++ (String.repeat grid.cols "---+") ++ "\n" ++
       String.concat (List.map rowToStrings [1..grid.rows])


