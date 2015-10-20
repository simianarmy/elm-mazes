module Dijkstra where

import Distances exposing (..)
import Cell exposing (Cell)

type alias DijkstraIter = {
    grid: Grid,
    frontier : List Cell,
    newFrontier : List Cell,
    dists : Distances
}

-- process a link
scanCell : Cell -> Cell -> DijkstraIter -> DijkstraIter
scanCell cell linked diter =
    if not (lookup diter.dists linked == 0)
       then diter
       else
       let curDist = lookup diter.dists cell
       in 
          {
              diter |
              newFrontier <- List.append diter.newFrontier [linked],
              dists <- add diter.dists linked (curDist + 1)
          }

-- iterate over each cell link
scanCellLinks : Cell -> DijkstraIter -> DijkstraIter
scanCellLinks cell diter =
    List.foldl (scanCell cell) diter <| linked diter.grid cell

-- iterate over each frontier
scanFrontier : DijkstraIter -> DijkstraIter
scanFrontier diter =
    let res = List.foldl scanCellLinks diter diter.frontier
    in
       {res | frontier <- res.newFrontier}

-- recursively scan frontiers
frontierAcc : DijkstraIter -> DijkstraIter
frontierAcc diter =
    if List.isEmpty diter.frontier
       then diter
       else
       let acc = {diter | newFrontier <- []}
       in
          frontierAcc <| scanFrontier acc

-- the public api
-- TODO: move the rest inside here
cellDistances : Grid -> Cell -> Distances
cellDistances grid cell =
    let acc = {
        grid = grid, 
        d = Distances.init cell,
        frontier = [cell], 
        newFrontier = []
    }
    in
       .dists <| frontierAcc acc

