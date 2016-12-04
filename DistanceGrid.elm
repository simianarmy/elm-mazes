module DistanceGrid exposing (..)

import Grid exposing (Grid, linkedCells, toAscii)
import GridCell exposing (..)
import Cell exposing (Cell)
import Dijkstra
import Distances exposing (Distances, lookup)
import IntToBaseX exposing (toBaseX)

import Html exposing (..)

type alias CellDistances a =
    { grid : Grid a
    , dists : Distances
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
distances : Grid a -> GridCell -> Distances
distances grid root =
    Dijkstra.cellDistances grid (GridCell.base root)

cellToAscii : CellDistances a -> GridCell -> String
cellToAscii dgrid cell =
    let dist = lookup dgrid.dists (GridCell.base cell)
    in
       if dist == -1
          then Grid.cellToAscii dgrid.grid cell
          else toBaseX dist 36

---- distances view
viewDistances : CellDistances a -> String
viewDistances dgrid =
    Grid.toAscii dgrid.grid (cellToAscii dgrid)

---- Finds shortest path between 2 cells
-- Uses Distances type
pathTo : CellDistances a -> GridCell -> GridCell -> Distances
pathTo dgrid gcroot gcgoal =
    let root = GridCell.base gcroot
        goal = GridCell.base gcgoal
        dgrid' = createGrid dgrid.grid gcroot
        current = goal
        breadcrumbs = Distances.add (Distances.init root) current (lookup dgrid'.dists current)

        walkPath : Distances -> Cell -> Distances
        walkPath xpbreadcrumbs xpcurrent =
            if xpcurrent.id == root.id
               then xpbreadcrumbs
               else
               -- scan each linked cell
               let links = Grid.gridCellsToBaseCells <| Grid.linkedCells dgrid'.grid (RectCellTag xpcurrent)
                   currentDistance = lookup dgrid'.dists xpcurrent
                   res = List.filter (\neighbor ->
                       (lookup dgrid'.dists neighbor) < currentDistance
                   ) links
               in
                  if List.isEmpty res
                     then xpbreadcrumbs
                     else
                     let neighbor = Grid.toValidCell <| List.head res
                         ixpbreadcrumbs = Distances.add xpbreadcrumbs neighbor (lookup dgrid'.dists neighbor)
                     in
                        walkPath ixpbreadcrumbs neighbor

    in
       walkPath breadcrumbs current

---- Finds longest path from a cell
longestPath : CellDistances a -> GridCell -> Distances
longestPath dgrid root =
    let dgrid' = createGrid dgrid.grid root
        (cellId, foo) = Distances.max dgrid'.dists
        newStartCell = Grid.cellIdToCell dgrid.grid cellId
        dgrid'' = createGrid dgrid.grid newStartCell
        (goalId, foo') = Distances.max dgrid''.dists
        goal = Grid.cellIdToCell dgrid.grid goalId
    in
       pathTo dgrid newStartCell goal
