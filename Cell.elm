module Cell where

import Set exposing (..)

-- MODEL

type alias CellID = (Int,Int)
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

-- helper to create a nil cell
createNilCell = createCell -1 -1

-- generate a unique id string
createCellID : Int -> Int -> CellID
createCellID a b =
    --toString a ++ ":" ++ toString b
    (a, b)

-- returns all linked cells
linked : Cell -> Set CellID
linked cell =
    cell.links

-- returns if cells are linked
isLinked : Cell -> Cell -> Bool
isLinked cell1 cell2 =
    Set.member cell2.id cell1.links

hasLinks : Cell -> Bool
hasLinks cell =
    not <| Set.isEmpty cell.links

cellToString : Cell -> String
cellToString cell =
    "(" ++ (toString cell.row) ++ ", " ++ (toString cell.col) ++ ")"


