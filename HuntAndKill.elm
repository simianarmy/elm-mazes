-- Module defining the Hunt & Kill maze creation algorithm
module HuntAndKill (on) where


import Grid exposing (Grid)
import Cell exposing (Cell)
import GridUtils

import Random
import Array
import List exposing (..)
import Trampoline exposing (..)
import Debug exposing (log)

on : Grid {} -> Grid {}
on grid =
    -- start at a random cell in the grid
    let (grid', startCell) = Grid.randomCell grid

    in
       trampoline (walkRandomly grid' startCell)

-- Breaking out to try trampoline
walkRandomly : Grid {} -> Maybe Cell -> Trampoline (Grid {})
walkRandomly grid cell =
    if not <| isValidCell cell
       Done grid
       else
       let unvisitedNeighbors = filter (\c -> not <| Cell.hasLinks c)
           <| Grid.neighbors grid cell

       in
          if not <| isEmpty unvisitedNeighbors
             then
             -- random walk phase
             let neighbor = GridUtils.sampleCell unvisitedNeighbors grid.rnd
                 grid' = Grid.linkCells cell neighbor
             in
                Continue (\() -> walkRandomly grid' neighbor)
             else
             -- hunt phase
             let (grid', current) = hunt grid'
             in
                Continue (\() -> walkRandomly grid' current)


hunt : Grid {} -> (Grid {}, Maybe Cell)
hunt grid =
    let visitedNeighbors cell = filter (\c -> Cell.hasLinks c) <| Grid.neighbors grid cell
        huntUnvisitedNeighbor cell =
            not (isEmpty (visitedNeighbors c)) || (Cell.hasLinks c)

        huntedCells = filter huntUnvisitedNeighbor grid.cells
    in
       case fst huntedCells of
          -- no results, return an invalid cell
          Nothing -> (grid, Nothing)
          Just a -> (
              (Grid.linkCells a (GridUtils.sampleCell (visitedNeighbors a) grid.rnd))
              , a)

