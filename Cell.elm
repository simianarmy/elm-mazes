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
    -- if cell2's id is in cell1' set
    --case cell2 of
        -- Nothing -> False
        -- Just cell2 -> Set.member cell2.id cell1.links
    Set.member cell2.id cell1.links


-- returns all cell neighbors
--neighbors : Cell -> List
-- neighbors cell =
--     concat [north cell, south cell, east cell, west cell]
--
cellToString : Cell -> String
cellToString cell =
    "(" ++ toString cell.row ++ ", " ++ toString cell.col ++ ")"
