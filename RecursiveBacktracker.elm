-- Module defining the Recursive Backtracker maze creation algorithm
module RecursiveBacktracker (on) where

import Grid exposing (Grid)
import PolarGrid
import HexGrid
import GridCell exposing (..)
import Cell exposing (Cell)
import GridUtils

import Random
import Array
import List exposing (..)
import List.Extra as LE
import Trampoline exposing (..)
import Debug exposing (log)

on : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a ->
     Grid a
on startCellFn neighborsFn grid =
    let grid' = Grid.updateRnd grid
        gcell = GridCell.maybeGridCellToGridCell <| startCellFn grid
    in
       trampoline (walkRandomly grid' neighborsFn [gcell])


walkRandomly : Grid a ->
    (Grid a -> GridCell -> List GridCell) ->
    List GridCell ->
    Trampoline (Grid a)
walkRandomly grid neighborsFn stack =
    if isEmpty stack
       then Done grid
       else
       let current = head stack |> GridCell.maybeGridCellToGridCell
           -- foo = case current of
           --     HexCellTag c -> current
           --     _ -> Debug.crash "here is the failuer"
           -- Get the proper filter function
           neighbors = Grid.filterNeighbors2 neighborsFn (\c -> not <| Cell.hasLinks (GridCell.base c)) grid current
       in
           if isEmpty neighbors
              then Continue (\() -> walkRandomly grid neighborsFn (Maybe.withDefault [] (tail stack)))
              else
              let neighbor = GridUtils.sampleCell neighbors grid.rnd
                           |> GridCell.maybeGridCellToGridCell
                  grid' = Grid.linkCells grid current neighbor True
                  grid'' = Grid.updateRnd grid'
              in
                  Continue (\() -> walkRandomly grid'' neighborsFn (neighbor :: stack))

