-- Module defining the Aldous-Broder maze creation algorithm
module AldousBroder where

import Grid exposing (..)
import Cell exposing (Cell)

import Random
import List exposing (..)
import Debug exposing (log)

on : Grid {} -> Grid {}
on grid =
    -- start at a random cell in the grid
    let (grid', startCell) = Grid.randomCell grid

        walkRandomly : Grid {} -> Cell -> Int -> Grid {}
        walkRandomly grid'' cell unvisited =
            if unvisited == 0
               then grid''
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
                        walkRandomly grid''' neighbor (unvisited - 1)

                    else
                        -- move to the neighbor w/out linking
                        walkRandomly (updateRnd grid'') neighbor unvisited
    in
       walkRandomly grid' startCell ((size grid) - 1)
