module DistanceGrid where

import Grid exposing (Grid, linkedCells, toAscii)
import Cell exposing (Cell)
import Dijkstra
import Distances exposing (Distances, lookup)
import IntToBaseX exposing (toBaseX)

import Html exposing (..)

type alias CellDistances a = {
    a |
        dists : Distances
}

-- Creates grid with distances from Grid
--createDistanceGrid : Grid a -> Cell -> CellDistances (Grid a)
createDistanceGrid grid root =
    {grid |
        dists = distances grid root
    }

-- Returns all distances from a root cell
--distances : Grid a -> Cell -> Distances
distances grid root =
    Dijkstra.cellDistances grid root

cellToAscii : CellDistances (Grid a) -> Cell -> String
cellToAscii dgrid cell =
    let dist = lookup dgrid.dists cell
    in
       if dist == -1
          then Grid.cellToAscii dgrid cell
          else toBaseX dist 36

-- distances view
--viewDistances : CellDistances (Grid a) -> String
viewDistances dgrid =
    toAscii cellToAscii dgrid

-- Finds shortest path between 2 cells
--pathTo : Grid a -> Cell -> Cell -> Distances
pathTo grid root goal =
    let dgrid = createDistanceGrid grid root
        current = goal
        breadcrumbs = Distances.add (Distances.init root) current (lookup dgrid.dists current)

        walkPath : Distances -> Cell -> Distances
        walkPath xpbreadcrumbs xpcurrent =
            if xpcurrent.id == root.id
               then xpbreadcrumbs
               else
               -- scan each linked cell
               let links = Grid.linkedCells grid xpcurrent
                   currentDistance = lookup dgrid.dists xpcurrent
                   res = List.filter (\neighbor ->
                       (lookup dgrid.dists neighbor) < currentDistance
                   ) links
               in
                  if List.isEmpty res
                     then xpbreadcrumbs
                     else
                     let neighbor = Grid.toValidCell <| List.head res
                         ixpbreadcrumbs = Distances.add xpbreadcrumbs neighbor (lookup dgrid.dists neighbor)
                     in
                        walkPath ixpbreadcrumbs neighbor

    in
       walkPath breadcrumbs current

-- Finds longest path from a cell
--longestPath : Grid a -> Cell -> Distances
longestPath grid root =
    let dgrid = createDistanceGrid grid root
        (cellId, foo) = Distances.max dgrid.dists
        newStartCell = Grid.cellIdToCell grid cellId
        dgrid' = createDistanceGrid grid newStartCell
        (goalId, foo') = Distances.max dgrid'.dists
        goal = Grid.cellIdToCell grid goalId
    in
       pathTo grid newStartCell goal
