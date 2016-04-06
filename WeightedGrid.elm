module WeightedGrid where

import Distances exposing (Distances)
import DistanceGrid exposing (CellDistances)
import GridCell exposing (GridCell)
import Grid

import Color exposing (Color, rgb)

-- this means we have a dists property
type alias Weighted a = {
    a |
        dists : Distances,
        maximum : Int
    }

createGrid grid root =
    let grid' = {grid |
            dists = distances grid root
        }
        (farthest, max) = Distances.max grid'.dists
    in
       {grid' | maximum = max}

cellBackgroundColor : Weighted a -> GridCell -> Color
cellBackgroundColor grid gc =
    if (GridCell.base gc).weight > 1
       then Color.rgb 255 0 0
       else
       let distance = Distances.lookup grid.dists (GridCell.base gc)
           distance' = if distance == -1 then 0 else distance
           intensity = 64 + 191 * (grid.maximum - distance') // grid.maximum
       in
          Color.rgb intensity intensity 0

-- Accumulator for distances function
type alias Diter = {
    curCell : GridCell,
    weights : Distances,
    pending : List GridCell
}

-- Our modified Dijkstra's returns a Distances type
distances grid root =
    let weights = Distances.init (GridCell.base root)
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
                  pendingAcc <| List.foldl scanCellLinks acc' (Grid.linkedCells grid cell)

    in
       .weights (pendingAcc acc)


