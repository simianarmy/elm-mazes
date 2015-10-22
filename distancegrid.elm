module DistanceGrid where

import Grid exposing (Grid, toAscii)
import Cell exposing (Cell)
import Dijkstra
import Distances exposing (Distances)
import IntToBaseX exposing (toBaseX)

import Html exposing (..)

type alias CellDistances a = {
    a|
        dists: Distances
}

-- Creates grid with distances from Grid
createDistanceGrid : Grid {} -> Cell -> CellDistances (Grid {})
createDistanceGrid grid root =
    {grid |
        dists = distances grid root
    }

-- Returns all distances from a root cell
distances : Grid {} -> Cell -> Distances
distances grid root =
    Dijkstra.cellDistances grid root

cellToAscii : CellDistances (Grid {}) -> Cell -> String
cellToAscii dgrid cell =
    let dist = Distances.lookup dgrid.dists cell
    in
       if dist == -1
          then Grid.cellToAscii dgrid cell
          else toBaseX dist 36

-- distances view
viewDistances : Grid {} -> Cell -> Html
viewDistances grid root =
    let dgrid = createDistanceGrid grid root
    in
       div [] [
           pre [] [text <| toAscii cellToAscii dgrid]
           ]

