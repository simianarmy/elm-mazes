-- Module defining the Aldous-Broder maze creation algorithm
module AldousBroder where

import Grid exposing (..)
import Cell exposing (Cell)

import Random
import List exposing (..)
import Trampoline exposing (..)
import Debug exposing (log)

on : Grid {} -> Grid {}
on grid =
    -- start at a random cell in the grid
    let (grid', startCell) = Grid.randomCell grid

    in
       trampoline (walkRandomly grid' startCell ((size grid) - 1))

-- Breaking out to try trampoline
walkRandomly : Grid {} -> Cell -> Int -> Trampoline (Grid {})
walkRandomly grid'' cell unvisited =
    if unvisited == 0
       then Done grid''
       else
       -- Pick a random neighbor of cell
       let sample = neighbors grid'' cell
           (rand, seed) = Random.generate (Random.int 1 (length sample)) grid''.rnd.seed
           neighbor = toValidCell <| head (reverse (take rand sample))
       in
          -- if neighbor has no links
          if not <| Cell.hasLinks neighbor
             then
             -- link cell to neighbor and move to the neighbor
             let grid''' = updateRnd <| linkCells grid'' cell neighbor True
             in
                Continue (\() -> walkRandomly grid''' neighbor (unvisited - 1))
             else
             -- move to the neighbor w/out linking
             Continue (\() -> walkRandomly (updateRnd grid'') neighbor unvisited)

