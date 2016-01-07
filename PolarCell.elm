module PolarCell where

import Cell exposing (Cell)
import Set exposing (Set)

type alias PolarCell = {
    cell: Cell,
    outward: PolarCells
}

type PolarCells = PolarCells (List PolarCell)

createCell : Int -> Int -> PolarCell
createCell row col =
    {
        cell = Cell.createCell row col
        , outward = []
    }

-- returns clockwise cell
cw : PolarCell -> Maybe PolarCell
cw cell =
    Nothing

-- returns counterclockwise cell
ccw : PolarCell -> Maybe PolarCell
ccw cell =
    Nothing

-- returns inward cell
inward : PolarCell -> Maybe PolarCell
inward cell =
    Nothing

neighbors : PolarCell -> PolarCells
neighbors cell =
    [cw cell, ccw cell, inward cell] :: cell.outward
