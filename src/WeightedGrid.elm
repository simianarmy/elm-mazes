module WeightedGrid exposing (..)

import Distances exposing (Distances, lookup)
import DistanceGrid exposing (CellDistances)
import DijkstraWeighted
import GridCell exposing (..)
import Cell exposing (Cell)
import Grid exposing (Grid)
import ColoredGrid

import Color exposing (Color, rgb)

type alias Weighted =
    { dgrid : CellDistances,
      dists : Distances,
      maximum : Int
  }

createGrid : Grid -> GridCell -> Weighted
createGrid grid start =
    let dg = DistanceGrid.createGrid grid start
        ds = DijkstraWeighted.cellDistances dg start
        (farthest, max) = Distances.max ds
    in
       {
           dgrid = dg,
           dists = ds,
           maximum = max
       }

---- Finds shortest path between 2 cells
-- Uses Distances type
pathTo : Weighted -> GridCell -> Distances
pathTo wgrid gcgoal =
    let root = wgrid.dists.root
        current = GridCell.base gcgoal
        breadcrumbs = Distances.add (Distances.init root) current (lookup wgrid.dists current)

        walkPath : Distances -> Cell -> Distances
        walkPath xpbreadcrumbs xpcurrent =
            if xpcurrent.id == root.id
               then xpbreadcrumbs
               else
               -- scan each linked cell
               let links = Grid.gridCellsToBaseCells <| Grid.linkedCells wgrid.dgrid.grid (RectCellTag xpcurrent)
                   currentDistance = lookup wgrid.dists xpcurrent
                   res = List.filter (\neighbor ->
                       (lookup wgrid.dists neighbor) < currentDistance
                   ) links
               in
                  if List.isEmpty res
                     then xpbreadcrumbs
                     else
                     let neighbor = Grid.toValidCell <| List.head res
                         ixpbreadcrumbs = Distances.add xpbreadcrumbs neighbor (lookup wgrid.dists neighbor)
                     in
                        walkPath ixpbreadcrumbs neighbor

    in
       walkPath breadcrumbs current


cellBackgroundColor : Weighted -> GridCell -> Color
cellBackgroundColor wgrid gc =
    let bc = GridCell.base gc
    in
        if bc.weight > 1
        then Color.rgb 255 0 0
        else
            let distance = Distances.lookup wgrid.dists bc
                distance_ = if distance == -1 then 0 else distance
                intensity = 64 + 191 * (wgrid.maximum - distance_) // wgrid.maximum
            in
                if distance == -1 then Color.white
                else Color.rgb intensity intensity 0

