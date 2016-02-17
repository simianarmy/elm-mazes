-- Module defining the Aldous-Broder maze creation algorithm
module AldousBroder (on) where

import Grid exposing (Grid)
import PolarGrid
import GridCell exposing (..)
import Cell exposing (Cell)
import GridUtils

import Random
import List
import Array
import Trampoline exposing (..)
import Debug exposing (log)

on : (Grid a -> Maybe GridCell) -> Grid a -> Grid a
on startCellFn grid =
    let grid' = Grid.updateRnd grid
        startCell = Grid.maybeGridCellToGridCell <| startCellFn grid
    in
       trampoline (walkRandomly grid' startCell ((Grid.size grid) - 1))

-- Breaking out to try trampoline
walkRandomly : Grid a -> GridCell -> Int -> Trampoline (Grid a)
walkRandomly grid cell unvisited =
    if unvisited == 0
       then Done grid
       else
       -- Pick a random neighbor of cell
       let sample = case cell of
               RectCellTag rc -> Grid.neighbors grid cell
               PolarCellTag pc -> PolarGrid.neighbors grid cell
           -- gridcell
           gcneighbor = Grid.maybeGridCellToGridCell <| GridUtils.sampleCell sample grid.rnd
           -- basecell
           neighbor = GridCell.toRectCell gcneighbor
       in
          -- if neighbor has no links
          if not <| Cell.hasLinks neighbor
             then
             -- link cell to neighbor and move to the neighbor
             let grid' = Grid.updateRnd <| Grid.linkCells grid cell gcneighbor True
             in
                Continue (\() -> walkRandomly grid' gcneighbor (unvisited - 1))
             else
             -- move to the neighbor w/out linking
             Continue (\() -> walkRandomly (Grid.updateRnd grid) gcneighbor unvisited)

