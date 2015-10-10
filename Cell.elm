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

-- generate a unique id string
createCellID : Int -> Int -> CellID
createCellID a b =
    toString a ++ ":" ++ toString b

-- link 2 cells
linkCell : Cell -> Cell -> Bool -> (Cell, Cell)
linkCell cell cellToLink bidi =
    let updatedCell : Cell -> Cell -> Cell
        updatedCell cell1 cell2 =
            {cell1 | links <- insert cell2.id cell1.links}
    in
       if bidi then
          (updatedCell cell cellToLink, updatedCell cellToLink cell)
          else
          (updatedCell cell cellToLink, cellToLink)

-- unlink 2 cells w/ optional bidirectional flag
unlinkCell : Cell -> Cell -> Bool -> (Cell, Cell)
unlinkCell cell cellToUnlink bidi =
    let updatedCell : Cell -> Cell -> Cell
        updatedCell cell1 cell2 =
            {cell1 | links <- remove cell2.id cell1.links}
    in
       if bidi then
          (updatedCell cell cellToUnlink, updatedCell cellToUnlink cell)
          else
          (updatedCell cell cellToUnlink, cellToUnlink)

-- returns all linked cells
linked : Cell -> Set CellID
linked cell =
    cell.links

-- returns if cells are linked
isLinked : Cell -> Cell -> Bool
isLinked cell1 cell2 =
    -- if cell2's id is in cell1' set
    Set.member cell2.id cell1.links

-- returns all cell neighbors
--neighbors : Cell -> List
-- neighbors cell =
--     concat [north cell, south cell, east cell, west cell]
--
cellToString : Cell -> String
cellToString cell =
    "(" ++ toString cell.row ++ ", " ++ toString cell.col ++ ")"
