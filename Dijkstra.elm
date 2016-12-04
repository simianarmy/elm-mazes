module Dijkstra exposing (cellDistances)

import Distances exposing (Distances)
import Cell exposing (Cell)
import GridCell exposing (..)
import Grid exposing (Grid)

type alias DijkstraIter a = {
    dists : Distances,
    grid: Grid a,
    frontier : List Cell,
    newFrontier : List Cell
}

-- the public api
cellDistances : Grid a -> Cell -> Distances
cellDistances grid cell =

    -- process a link
    let scanCell : Cell -> Cell -> DijkstraIter a -> DijkstraIter a
        scanCell cell linked diter =
            if not ((Distances.lookup diter.dists linked) == -1)
               then diter
               else
               let curDist = Distances.lookup diter.dists cell
               in
                  {
                      diter |
                      newFrontier = List.append diter.newFrontier [linked],
                      dists = Distances.add diter.dists linked (curDist + 1)
                  }

        -- iterate over each cell link
        scanCellLinks : Cell -> DijkstraIter a -> DijkstraIter a
        scanCellLinks cell diter =
            -- force a cell to a gridcell for linkedCells to work
            List.foldl (scanCell cell) diter <|
            Grid.gridCellsToBaseCells <|
            Grid.linkedCells diter.grid (RectCellTag cell)

        -- iterate over each frontier
        scanFrontier : DijkstraIter a -> DijkstraIter a
        scanFrontier diter =
            let res = List.foldl scanCellLinks diter diter.frontier
            in
               {res | frontier = res.newFrontier}

        -- recursively scan frontiers
        frontierAcc : DijkstraIter a -> DijkstraIter a
        frontierAcc diter =
            if List.isEmpty diter.frontier
               then diter
               else
               let acc = {diter | newFrontier = []}
               in
                  frontierAcc <| scanFrontier acc

        acc = {
            grid = grid,
            dists = Distances.init cell,
            frontier = [cell],
            newFrontier = []
        }
       in
          .dists (frontierAcc acc)
