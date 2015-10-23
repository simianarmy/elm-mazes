module DistanceGrid where

import Grid exposing (Grid, linkedCells, toAscii)
import Cell exposing (Cell)
import Dijkstra
import Distances exposing (Distances, lookup)
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
    let dist = lookup dgrid.dists cell
    in
       if dist == -1
          then Grid.cellToAscii dgrid cell
          else toBaseX dist 36

-- distances view
viewDistances : CellDistances (Grid {}) -> String
viewDistances dgrid =
    toAscii cellToAscii dgrid

-- Finds shortest path between 2 cells
pathTo : Grid {} -> Cell -> Cell -> Distances
pathTo grid root goal =
    let dgrid = createDistanceGrid grid root
        current = goal
        breadcrumbs = Distances.add (Distances.init root) current (lookup dgrid.dists current)

        walkPath : Distances -> Cell -> Distances
        walkPath breadcrumbs' current' =
            if current'.id == root.id
               then breadcrumbs' 
               else
               -- scan each linked cell
               let links = Grid.linkedCells grid current'
                   currentDistance = lookup dgrid.dists current'
                   res = List.filter (\neighbor ->
                       (lookup dgrid.dists neighbor) < currentDistance
                   ) links
               in
                  if List.isEmpty res
                     then breadcrumbs'
                     else
                     let neighbor = Grid.toValidCell <| List.head res
                         breadcrumbs'' = Distances.add breadcrumbs' neighbor (lookup dgrid.dists neighbor)
                     in
                        walkPath breadcrumbs'' neighbor

    in
       walkPath breadcrumbs current
