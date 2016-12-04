-- kind of useful functions for grids
-- there should be more in here
module GridUtils exposing (..)

import Rnd
import GridCell exposing (GridCell)
import Cell exposing (Cell)

import Random
import Random.List as RL
import List
import ListUtils
import Array
import String

sampleCell : List GridCell -> Rnd.GridRnd -> Maybe GridCell
sampleCell sample rnd =
    let generator = RL.choose sample
        res = Tuple.first <| Random.step generator rnd.seed
    in
        Tuple.first res

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

cellsToString : List GridCell -> String
cellsToString cells =
    String.join "," <| List.map GridCell.toString cells
