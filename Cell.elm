module Cell where

import Set exposing (..)

-- MODEL

type alias CellID = String
type alias Cell = {
    id: CellID,
    row: Int,
    col: Int,
    links: Set CellID
}

-- create cell at row col
createCell : Int -> Int -> Cell
createCell row col =
    {
        id = createCellID row col,
        row = row,
        col = col,
        links = Set.empty
    }

-- generate a unique id string
createCellID : Int -> Int -> CellID
createCellID a b =
    toString a ++ ":" ++ toString b

-- returns all linked cells
linked : Cell -> Set CellID
linked cell =
    cell.links

-- returns if cells are linked
isLinked : Cell -> Cell -> Bool
isLinked cell1 cell2 =
    Set.member cell2.id cell1.links

cellToString : Cell -> String
cellToString cell =
    "(" ++ (toString cell.row) ++ ", " ++ (toString cell.col) ++ ")"


