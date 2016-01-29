module PolarCell where

import Cell exposing (Cell, CellID)

import Set exposing (..)

-- MODEL

-- Standard cell
type alias PolarCell = {
    id: CellID,
    row: Int,
    col: Int,
    masked: Bool,
    links: Set CellID,
    outward: Set CellID
}

createCell : Int -> Int -> PolarCell
createCell row col =
    {
        id = Cell.createCellID row col,
        row = row,
        col = col,
        masked = False,
        links = Set.empty,
        outward = Set.empty
    }
