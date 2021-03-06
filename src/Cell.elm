module Cell exposing (..)

import Set exposing (..)

-- MODEL

type alias CellID = (Int,Int)
type alias CellLinks = Set CellID

type alias BaseCell = {
    id: CellID,
    row: Int,
    col: Int,
    masked: Bool,
    links: CellLinks,
    weight: Int,
    visited: Bool,
    tag: String
}

-- Standard cell
type alias Cell = BaseCell

-- create cell at row col (0,0) = origin
createCell : Int -> Int -> Cell
createCell row col =
    {
        id = createCellID row col,
        row = row,
        col = col,
        masked = False,
        links = Set.empty,
        weight = 1,
        visited = False,
        tag = ""
    }

-- helper to create a nil cell
createMaskedCell : Int -> Int -> Cell
createMaskedCell row col =
    let cell = createCell row col
    in
       {cell | masked = True}

createNilCell : Cell
createNilCell =
    createCell -1 -1

-- generate a unique id string
createCellID : Int -> Int -> CellID
createCellID a b =
    --toString a ++ ":" ++ toString b
    (a, b)

isNilCellID : CellID -> Bool
isNilCellID (r, c) =
    r == -1 && c == -1

isNilCell : Cell -> Bool
isNilCell cell =
    cell.row == -1 && cell.col == -1

-- returns all linked cells
linked : Cell -> Set CellID
linked cell =
    cell.links

-- is cell a nil cell?
isMasked : Maybe Cell -> Bool
isMasked cell =
    case cell of
        Nothing -> True
        Just cell -> cell.masked

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

