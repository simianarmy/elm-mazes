module Grid where

import Cell exposing (..)
import Set
import List
import String
import Random exposing (..)

type alias Grid = {rows: Int, cols: Int, cells: List Cell, seed : Seed}

createGrid : Int -> Int -> Grid
createGrid rows cols =
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
           seed = initialSeed 31415
       }

getCell : Grid -> Int -> Int -> Maybe Cell
getCell grid row col =
    if (row > grid.rows || col > grid.cols || row <= 0 || col <= 0)
       then Nothing
   else
        List.head (List.reverse (List.take ((grid.cols * (row - 1)) + col) grid.cells))

toValidCell : Maybe Cell -> Cell
toValidCell cell =
    case cell of
        Nothing -> createCell -1 -1
        Just cell -> cell

north : Grid -> Cell -> Maybe Cell
north grid cell =
    getCell grid (cell.row - 1) cell.col

south : Grid -> Cell -> Maybe Cell
south grid cell =
    getCell grid (cell.row + 1) cell.col

west : Grid -> Cell -> Maybe Cell
west grid cell =
    getCell grid cell.row (cell.col - 1)

east : Grid -> Cell -> Maybe Cell
east grid cell =
    getCell grid cell.row (cell.col + 1)

neighbors : Grid -> Cell -> List Cell
neighbors grid cell =
    let n = north grid cell
        s = south grid cell
        w = west grid cell
        e = east grid cell
    in 
       List.concat [(cellToList n), (cellToList s), (cellToList w), (cellToList e)]

-- link 2 cells
linkCells : Grid -> Cell -> Cell -> Bool -> Grid
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
                       then linkCell cellToLink cell
                       else c
    in
        {grid | cells <- List.map linkMatched grid.cells}

-- unlink 2 cells w/ optional bidirectional flag
unlinkCells : Grid -> Cell -> Cell -> Bool -> Grid
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

rowCells : Grid -> Int -> List Cell
rowCells grid row =
    List.filter (\c -> c.row == row) grid.cells

size : Grid -> Int
size grid =
    grid.rows * grid.cols

-- cardinal index of a cell in a grid (1,1) = 1, etc
cellIndex : Grid -> Cell -> Int
cellIndex grid cell =
    (grid.cols * (cell.row - 1)) + cell.col

-- Helper to make a maybe cell a list (empty if maybe)
cellToList : Maybe Cell -> List Cell
cellToList cell =
    case cell of
        Just cell -> [cell]
        Nothing -> []

toTitle : Grid -> String
toTitle grid =
    toString grid.rows ++ " X " ++ toString grid.cols ++ " Grid"

-- Returns ASCII representation of a grid
type alias RowAscii = {
    top : String, 
    bottom : String
}

gridToString : Grid -> String
gridToString grid =
    let cellToString : Cell -> RowAscii -> RowAscii
        cellToString cell ascii =
            let east_boundary = (if isLinked cell (toValidCell (east grid cell)) then "E" else "|")
                south_boundary = (if isLinked cell (toValidCell (south grid cell)) then "   " else "---")
                curtop = ascii.top
                curbottom = ascii.bottom
            in
               {
                   ascii |
                   top <- curtop ++ (String.repeat 3 " ") ++ east_boundary,
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


