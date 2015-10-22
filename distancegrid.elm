module DistanceGrid where

import Grid exposing (..)
import Cell exposing (Cell)
import Dijkstra
import Distances exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

type alias DistanceGrid = {
    grid : Grid,
    distances : Distances
}

-- Returns all distances from a root cell
distances : DistanceGrid -> Distances
distances dgrid =
    let rootCell = toValidCell <| Grid.getCell dgrid.grid 1 1
    in
        Dijkstra.cellDistances dgrid rootCell

distanceAsciiCell : DistanceGrid -> Cell -> String
distanceAsciiCell dgrid cell =
    let dist = Distances.lookup dgrid.distances cell
    in
       if dist == -1
          then plainAsciiCell dgrid.grid cell
          else toString dist

-- distances view
viewDistances grid =
    let grid' = {grid = grid, distances = distances grid}
    in
       div [] [
           pre [] [text <| toAscii distanceAsciiCell grid']
           ]

