module Grid where

import Cell exposing (..)
import Set
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
            let east_boundary = (if isLinked cell (east grid cell) then "E" else "|")
                south_boundary = (if isLinked cell (south grid cell) then " S " else "---")
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


