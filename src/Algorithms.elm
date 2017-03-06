module Algorithms exposing (..)

import Grid exposing (..)
import GridCell exposing (..)

-- Could be passed to the maze generator algorithms as a struct
type alias GridOperators = {
    startCell: Grid -> Maybe GridCell,
    neighbors: Grid -> GridCell -> List GridCell,
    linkCells: Grid -> GridCell -> GridCell -> Bool -> Grid
}
