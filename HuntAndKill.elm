-- Module defining the Hunt & Kill maze creation algorithm
module HuntAndKill (on, step) where

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

-- Processes a single cell (using single 1-based index for lookup)
-- step value shouldn't care about shape of the grid
step : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a -> Int ->
     Grid a
step startCellFn neighborsFn grid i =
    -- If first time through,
    let visited = List.filter (\c -> .visited (GridCell.base c)) <| Grid.cellsList grid.cells
    in
       if List.isEmpty visited
          then
          -- pick a random starting cell
          let startCell = GridCell.maybeGridCellToGridCell <| startCellFn grid
          -- and start a random walk
              grid' = Grid.updateRnd grid
          in
             grid'
         else 
         -- otherwise scan L-R until we encounter an unvisited cell, bordered by at least one visited cell. 
         let (grid', hunted) = hunt grid neighborsFn
         -- then start a random walk from the unvisited cell
         in
            grid'

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

