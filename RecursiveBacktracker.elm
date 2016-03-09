-- Module defining the Recursive Backtracker maze creation algorithm
module RecursiveBacktracker (on) where

import Grid exposing (Grid)
import PolarGrid
import GridCell exposing (..)
import Cell exposing (Cell)
import GridUtils

import Random
import Array
import List exposing (..)
import List.Extra as LE
import Trampoline exposing (..)
import Debug exposing (log)

on : (Grid a -> Maybe GridCell) -> Grid a -> Grid a
on startCellFn grid =
    let grid' = Grid.updateRnd grid
        gcell = GridCell.maybeGridCellToGridCell <| startCellFn grid
    in
       trampoline (walkRandomly grid' [gcell])


walkRandomly : Grid a -> List GridCell -> Trampoline (Grid a)
walkRandomly grid stack =
    if isEmpty stack
       then Done grid
       else
       let current = head stack |> GridCell.maybeGridCellToGridCell
           -- Get the proper filter function
           filterNeighborsFn = case current of
               PolarCellTag pc -> PolarGrid.filterNeighbors
               _ -> Grid.filterNeighbors
           neighbors = filterNeighborsFn (\c -> not <| Cell.hasLinks (GridCell.base c)) grid current
       in
           if isEmpty neighbors
              then Continue (\() -> walkRandomly grid (Maybe.withDefault [] (tail stack)))
              else
              let neighbor = GridUtils.sampleCell neighbors grid.rnd
                           |> GridCell.maybeGridCellToGridCell
                  grid' = Grid.linkCells grid current neighbor True
                  grid'' = Grid.updateRnd grid'
              in
                  Continue (\() -> walkRandomly grid'' (neighbor :: stack))

