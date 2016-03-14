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

on : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a -> Grid a
on startCellFn neighborsFn grid =
    let grid' = Grid.updateRnd grid
        startCell = GridCell.maybeGridCellToGridCell (startCellFn grid)
    in
       trampoline (walkRandomly grid' startCell neighborsFn)

-- Breaking out to try trampoline
walkRandomly : Grid a ->
    GridCell ->
    (Grid a -> GridCell -> List GridCell) ->
    Trampoline (Grid a)
walkRandomly grid gcell neighborsFn =
    let cell = GridCell.toRectCell gcell
    in
       if Cell.isNilCell cell
          then Done grid
          else
          let unvisitedNeighbors = Grid.filterNeighbors2 neighborsFn (\c -> not <| Cell.hasLinks (GridCell.toRectCell c)) grid gcell
          in
             if not <| isEmpty unvisitedNeighbors
                then
                -- random walk phase
                let neighbor = GridUtils.sampleCell unvisitedNeighbors grid.rnd
                    |> GridCell.maybeGridCellToGridCell
                    grid' = Grid.linkCells grid gcell neighbor True
                    grid'' = Grid.updateRnd grid'
                in
                   Continue (\() -> walkRandomly grid'' neighbor neighborsFn)
               else
               -- hunt phase
               let (grid', current) = hunt grid neighborsFn
               in
                  Continue (\() -> walkRandomly grid' current neighborsFn)


hunt : Grid a ->
     (Grid a -> GridCell -> List GridCell) ->
    (Grid a, GridCell)
hunt grid neighborsFn =
    let visitedNeighbors : GridCell -> List GridCell
        visitedNeighbors cell = Grid.filterNeighbors2 neighborsFn (\c -> Cell.hasLinks (GridCell.toRectCell c)) grid cell

        huntUnvisitedNeighbor : GridCell -> Bool
        huntUnvisitedNeighbor gcell =
            (not <| isEmpty (visitedNeighbors gcell)) && (not <| Cell.hasLinks (GridCell.toRectCell gcell))

        huntedCell = LE.find huntUnvisitedNeighbor <| Grid.cellsList grid.cells
    in
       case huntedCell of
          -- no results, return an invalid cell
          Nothing -> (grid, (RectCellTag Cell.createNilCell))
          Just a ->
              let linked = GridUtils.sampleCell (visitedNeighbors a) grid.rnd
                         |> GridCell.maybeGridCellToGridCell
              in
                 ((Grid.linkCells (Grid.updateRnd grid) a linked True), a)

