module Distances exposing (..)

import Cell exposing (Cell, CellID)
import Dict exposing (Dict)

type alias Distances = {
    root : Cell,
    cells : Dict CellID Int
}

init : Cell -> Distances
init cell =
    {root = cell, cells = Dict.singleton cell.id 0}

lookup : Distances -> Cell -> Int
lookup dists cell =
    Maybe.withDefault -1 (Dict.get cell.id dists.cells)

add : Distances -> Cell -> Int -> Distances
add dists cell dist =
    {dists | cells = Dict.insert cell.id dist dists.cells}

-- Returns cell id and distance of largest distance
max : Distances -> (CellID, Int)
max dists =
    let maxDist = List.reverse <| List.sortBy Tuple.second <| Dict.toList dists.cells
    in
       case List.head maxDist of
           Just d -> d
           Nothing -> (dists.root.id, 0)

cells : Distances -> List (Int, Int)
cells dists =
    Dict.keys dists.cells

