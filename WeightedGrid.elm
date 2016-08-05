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
    let dg = DistanceGrid.createGrid grid start
        ds = distances dg start
        (farthest, max) = Distances.max ds
    in
       {
           dgrid = dg,
           dists = ds,
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
    -- we use our Distances class to track the cost of each cell, but instead of having a frontier set, we now have a pending set, which tracks which cells have yet to be processed. We initialize that to be an array containing just self—the cell that we’re asking to compute the distances. We then repeat the following steps until that array is empty.
    let weights = dgrid.dists
        acc = {
            curCell = root,
            weights = weights,
            -- TODO: Use a priority queue for faster lookups
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

        -- Each pass through the loop will search that pending set, looking for the cell with the lowest cost and then removing the cell that it finds. This cell is our current cell.
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
                  -- The next loop looks at each of the cells that are linked to the current cell. For each one, we compute the cumulative weight of the path from the starting cell, and then check to see if that’s better than any previously recorded weight for that neighbor. If so, we add the neighbor to the pending list, and update its cumulative weight.
                  pendingAcc <| List.foldl scanCellLinks acc' (Grid.linkedCells dgrid.grid cell)

    in
       .weights (pendingAcc acc)


cellBackgroundColor : Weighted a -> GridCell -> Color
cellBackgroundColor wgrid gc =
    let bc = Debug.log "cell " <| GridCell.base gc
    in
        if bc.weight > 1
        then Color.rgb 255 0 0
        else
            let distance = Debug.log "distance " <| Distances.lookup wgrid.dists bc
                distance' = if distance == -1 then 0 else distance
                intensity = 64 + 191 * (wgrid.maximum - distance') // wgrid.maximum
            in
                if distance == -1 then Color.white
                else Color.rgb intensity intensity 0

