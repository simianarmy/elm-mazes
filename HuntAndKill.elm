-- Module defining the Hunt & Kill maze creation algorithm
module HuntAndKill (on) where

import Grid exposing (Grid)
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
    in
       trampoline (walkRandomly grid' <| Grid.maybeGridCellToGridCell (startCellFn grid))

-- Breaking out to try trampoline
walkRandomly : Grid a -> GridCell -> Trampoline (Grid a)
walkRandomly grid gcell =
    let cell = Grid.toRectCell gcell
    in
       if cell.row == -1
          then Done grid
          else
          let unvisitedNeighbors = Grid.filterNeighbors (\c -> not <| Cell.hasLinks (Grid.toRectCell c)) grid gcell
          in
             if not <| isEmpty unvisitedNeighbors
                then
                -- random walk phase
                let neighbor = GridUtils.sampleCell unvisitedNeighbors grid.rnd
                    |> Grid.maybeGridCellToGridCell
                    grid' = Grid.linkCells grid gcell neighbor True
                    grid'' = Grid.updateRnd grid'
                in
                   Continue (\() -> walkRandomly grid'' neighbor)
                   else
                   -- hunt phase
                   let (grid', current) = hunt grid
                   in
                      Continue (\() -> walkRandomly grid' current)


hunt : Grid a -> (Grid a, GridCell)
hunt grid =
    let visitedNeighbors cell = Grid.filterNeighbors (\c -> Cell.hasLinks (Grid.toRectCell c)) grid cell

        huntUnvisitedNeighbor : GridCell -> Bool
        huntUnvisitedNeighbor gcell =
            (not <| isEmpty (visitedNeighbors gcell)) && (not <| Cell.hasLinks (Grid.toRectCell gcell))

        huntedCell = LE.find huntUnvisitedNeighbor grid.cells
    in
       case huntedCell of
          -- no results, return an invalid cell
          Nothing -> (grid, (RectCellTag Cell.createNilCell))
          Just a ->
              let linked = GridUtils.sampleCell (visitedNeighbors a) grid.rnd
                         |> Grid.maybeGridCellToGridCell
              in
                 ((Grid.linkCells (Grid.updateRnd grid) a linked True), a)

