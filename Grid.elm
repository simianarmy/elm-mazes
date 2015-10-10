module Grid where

import Cell exposing (..)
import Set
import String

type alias Grid = {rows: Int, cols: Int, cells: List Cell}

createGrid : Int -> Int -> Grid
createGrid rows cols =
    let makeCell : Int -> Int -> Cell
        makeCell row col =
            {
                id = createCellID row col,
                row = row,
                col = col,
                links = Set.empty
            }

        makeRow : Int -> Int -> List Cell
        makeRow cols row =
            List.map
                (makeCell row)
                [0..cols-1]

    in
       -- loop rows times
       {
           rows = rows,
           cols = cols,
           cells = List.concatMap
               (makeRow cols) [0..rows-1]
       }

getCell : Grid -> Int -> Int -> Maybe Cell
getCell grid row col =
    List.head (List.take (grid.cols * row + col) grid.cells)

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


gridToText : Grid -> String
gridToText grid =
    String.concat
        (List.map cellToString grid.cells)

