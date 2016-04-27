module WeightedGrid where

import Distances exposing (Distances)
import DistanceGrid exposing (CellDistances)
import GridCell exposing (GridCell)
import Grid exposing (Grid)
import ColoredGrid exposing (Colored)

import Color exposing (Color, rgb)

type alias Weighted a =
    { dgrid : CellDistances a,
      dists : Distances,
      maximum : Int
  }

createGrid : Grid a -> GridCell -> Weighted a
createGrid grid start =
    let dg = DistanceGrid.createGrid grid start,
        ds = distances dg start
        (farthest, max) = Distances.max ds
    in
       {
           dgrid = dg,
           dists = ds
           maximum = max
       }

-- Accumulator for distances function
type alias Diter = {
    curCell : GridCell,
    weights : Distances,
    pending : List GridCell
}

-- Our modified Dijkstra's returns a Distances type
distances : CellDistances a -> GridCell -> Distances
distances dgrid root =
    -- create a CellDistances grid
    let weights = dgrid.dists
        acc = {
            curCell = root,
            weights = weights,
            pending = [root]
        }

        scanCellLinks : GridCell -> Diter -> Diter
        scanCellLinks neighbor acc =
            let ncell = GridCell.base neighbor
                totalWeight = (Distances.lookup acc.weights (GridCell.base acc.curCell)) + ncell.weight
                nWeight = Distances.lookup acc.weights ncell
            in
               if (nWeight < 0) || (totalWeight < nWeight)
                  then { acc |
                      weights = Distances.add acc.weights ncell totalWeight,
                      pending = List.append acc.pending [neighbor]
                  }
                  else acc

        pendingAcc : Diter -> Diter
        pendingAcc acc =
            if List.isEmpty acc.pending
               then acc
               else
               let sortedByWeight = List.sortBy (\c -> (GridCell.base c).weight) acc.pending
                   cell = GridCell.maybeGridCellToGridCell <| List.head sortedByWeight
                   cid = GridCell.id cell
                   -- remove cell from pending list
                   acc' = {acc |
                       curCell = cell,
                       pending = GridCell.filterGridCells (\c -> not (c.id == cid)) acc.pending
                   }
               in
                  pendingAcc <| List.foldl scanCellLinks acc' (Grid.linkedCells dgrid.grid cell)

    in
       .weights (pendingAcc acc)


cellBackgroundColor : Weighted a -> GridCell -> Color
cellBackgroundColor wgrid gc =
    if (GridCell.base gc).weight > 1
       then Color.rgb 255 0 0
       else
       let distance = Distances.lookup wgrid.dists (GridCell.base gc)
           distance' = if distance == -1 then 0 else distance
           intensity = 64 + 191 * (wgrid.maximum - distance') // wgrid.maximum
       in
          Color.rgb intensity intensity 0

