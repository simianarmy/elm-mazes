module DistanceGrid where

import Grid exposing (Grid, linkedCells, toAscii)
import GridCell exposing (..)
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
--createGrid : Grid a -> Cell -> CellDistances (Grid a)
createGrid grid root =
    let cellDistances = distances grid root
    in
       {grid |
           dists = cellDistances
       }

-- Returns all distances from a root cell
--distances : CellDistances (Grid a) -> Cell -> Distances
distances grid root =
    Dijkstra.cellDistances grid root

cellToAscii : CellDistances (Grid a) -> Cell -> String
cellToAscii dgrid cell =
    let dist = lookup dgrid.dists cell
    in
       if dist == -1
          then Grid.cellToAscii dgrid cell
          else toBaseX dist 36

---- distances view
viewDistances : CellDistances (Grid a) -> String
viewDistances dgrid =
    toAscii dgrid cellToAscii

---- Finds shortest path between 2 cells
pathTo : CellDistances (Grid a) -> Cell -> Cell -> Distances
pathTo grid root goal =
    let dgrid = createGrid grid root
        current = goal
        breadcrumbs = Distances.add (Distances.init root) current (lookup dgrid.dists current)

        walkPath : Distances -> Cell -> Distances
        walkPath xpbreadcrumbs xpcurrent =
            if xpcurrent.id == root.id
               then xpbreadcrumbs
               else
               -- scan each linked cell
               let links = Grid.gridCellsToBaseCells <| Grid.linkedCells grid (RectCellTag xpcurrent)
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

---- Finds longest path from a cell
longestPath : CellDistances (Grid a) -> Cell -> Distances
longestPath grid root =
    let dgrid = createGrid grid root
        (cellId, foo) = Distances.max dgrid.dists
        newStartCell = GridCell.toRectCell <| Grid.cellIdToCell grid cellId
        dgrid' = createGrid grid newStartCell
        (goalId, foo') = Distances.max dgrid'.dists
        goal = GridCell.toRectCell <| Grid.cellIdToCell grid goalId
    in
       pathTo grid newStartCell goal
