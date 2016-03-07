-- kind of useful functions for grids
-- there should be more in here
module GridUtils where

import Rnd
import GridCell exposing (GridCell)
import Cell exposing (Cell)

import Random
import List
import ListUtils
import Array

sampleCell : List GridCell -> Rnd.GridRnd -> Maybe GridCell
sampleCell sample rnd =
    let (rand, seed) = Random.generate (Random.int 0 ((List.length sample) - 1)) rnd.seed
    in
       Array.get rand (Array.fromList sample)

-- returns 0-based index of a cell in a list or -1 if not found
indexOfCell : GridCell -> List GridCell -> Int
indexOfCell cell cells =
    let id = GridCell.id cell
    in
        ListUtils.firstIndexOf id <| List.map GridCell.id cells

smooshMaybes : List (Maybe GridCell) -> List GridCell
smooshMaybes maybes =
    let cellToList cell =
        case cell of
            Just cell -> [cell]
            Nothing -> []
    in
        List.concat <| List.map cellToList maybes

