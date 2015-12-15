-- Module defining the Recursive Backtracker maze creation algorithm
module RecursiveBacktracker (on) where

import Grid exposing (Grid)
import Cell exposing (Cell)
import GridUtils

import Random
import Array
import List exposing (..)
import List.Extra as LE
import Trampoline exposing (..)
import Debug exposing (log)

on : (Grid a -> Cell) -> Grid a -> Grid a
on startCellFn grid =
    let grid' = Grid.updateRnd grid
    in
       trampoline (walkRandomly grid' [startCellFn grid])


walkRandomly : Grid a -> List Cell -> Trampoline (Grid a)
walkRandomly grid stack =
    if isEmpty stack
       then Done grid
       else
       let current = head stack |> Grid.toValidCell
           neighbors = Grid.filterNeighbors (\c -> not <| Cell.hasLinks c) grid current
       in
           if isEmpty neighbors
              then Continue (\() -> walkRandomly grid (Maybe.withDefault [] (tail stack)))
              else
              let neighbor = GridUtils.sampleCell neighbors grid.rnd
                           |> Grid.toValidCell
                  grid' = Grid.linkCells grid current neighbor True
                  grid'' = Grid.updateRnd grid'
              in
                  Continue (\() -> walkRandomly grid'' (neighbor :: stack))

