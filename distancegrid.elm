module DistanceGrid where

import Grid exposing (..)
import Cell exposing (Cell)
import Dijkstra
import Distances exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

type alias CellDistances a = {
    a|
        dists: Distances
}

-- Creates grid with distances from Grid
createDistanceGrid : Grid {} -> Cell -> CellDistances (Grid {})
createDistanceGrid grid root =
    let distances = Dijkstra.cellDistances grid root
    in
       {grid | dists = distances}

-- Returns all distances from a root cell
distances : Grid {} -> Cell -> Distances
distances grid rootCell =
    Dijkstra.cellDistances grid rootCell

distanceAsciiCell : CellDistances (Grid {}) -> Cell -> String
distanceAsciiCell dgrid cell =
    let dist = Distances.lookup dgrid.dists cell
    in
       if dist == -1
          then plainAsciiCell dgrid cell
          else toString dist

-- distances view
viewDistances : Grid {} -> Cell -> Html
viewDistances grid root =
    let dgrid = {grid | dists = distances grid root}
    in
       div [] [
           pre [] [text <| toAscii distanceAsciiCell dgrid]
           ]

