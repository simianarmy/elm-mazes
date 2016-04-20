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
        grid : Grid a,
        dists : Distances
    }

-- Creates grid with distances from Grid
createGrid : Grid a -> GridCell -> CellDistances a
createGrid grid root =
    let cellDistances = distances grid root
    in
       {
           grid = grid,
           dists = cellDistances
       }

-- Returns all distances from a root cell
distances : CellDistances a -> GridCell -> Distances
distances grid root =
    Dijkstra.cellDistances grid (GridCell.base root)

cellToAscii : CellDistances a -> Cell -> String
cellToAscii dgrid cell =
    let dist = lookup dgrid.dists cell
    in
       if dist == -1
          then Grid.cellToAscii dgrid cell
          else toBaseX dist 36

---- distances view
viewDistances : CellDistances a -> String
viewDistances dgrid =
    toAscii dgrid cellToAscii

---- Finds shortest path between 2 cells
pathTo : CellDistances a -> GridCell -> GridCell -> Distances
pathTo grid gcroot gcgoal =
    let root = GridCell.base gcroot
        goal = GridCell.base gcgoal
        dgrid = createGrid grid gcroot
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
longestPath : CellDistances a -> GridCell -> Distances
longestPath grid root =
    let dgrid = createGrid grid root
        (cellId, foo) = Distances.max dgrid.dists
        newStartCell = Grid.cellIdToCell grid cellId
        dgrid' = createGrid grid newStartCell
        (goalId, foo') = Distances.max dgrid'.dists
        goal = Grid.cellIdToCell grid goalId
    in
       pathTo grid newStartCell goal
