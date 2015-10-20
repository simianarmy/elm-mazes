module Distances where

import Cell exposing (Cell)
import Dict exposing (Dict)

type alias Distances = {
    root : Cell,
    cells : Dict String Int
}

init : Cell -> Distances
init cell =
    {root = cell, cells = Dict.singleton cell.id 0}

lookup : Distances -> Cell -> Int
lookup dists cell =
    Maybe.withDefault 0 (Dict.get cell.id dists.cells)

add : Distances -> Cell -> Int -> Distances
add dists cell dist =
    {dists | cells <- Dict.insert cell.id dist dists.cells}

cells : Distances -> List String
cells dists =
    Dict.keys dists.cells

