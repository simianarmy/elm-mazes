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

size : Grid -> Int
size grid =
    grid.rows * grid.cols

-- Helper to make a maybe cell a list (empty if maybe)
cellToList : Maybe Cell -> List Cell
cellToList cell =
    case cell of
        Just cell -> [cell]
        Nothing -> []

gridToString : Grid -> String
gridToString grid =
    String.concat
        (List.map cellToString grid.cells)

