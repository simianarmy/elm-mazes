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
       trampoline (work grid' startCell neighborsFn)

-- Processes a single cell (using single 1-based index for lookup)
-- step value shouldn't care about shape of the grid
step : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a -> Int ->
     Grid a
step startCellFn neighborsFn grid i =
    -- If first time through,
    let cells = Grid.cellsList grid.cells
        visited = GridCell.filterGridCells (\e -> e.visited) cells
    in
       if List.isEmpty visited
          then
          -- pick a random starting cell
          let startCell = GridCell.maybeGridCellToGridCell <| startCellFn grid
              grid' = Grid.updateRnd grid
          in
              -- and start a random walk
              randomWalk grid' startCell neighborsFn
         -- if hunt is over, begin walk
         else
         if List.any (\e -> (GridCell.base e).tag == "DEADEND") cells
            then
            -- erase the deadend tag
            let grid' = Grid.updateCells grid (\c -> GridCell.setTag c "")
                foo = Debug.log "FOUND DEADEND, TIME TO HUNT!"
            in
                fst <| hunt grid' neighborsFn
            else
            -- get the cell that the hunt produced
            let hunted = GridCell.maybeGridCellToGridCell <| head <| GridCell.filterGridCells (\e -> e.tag == "HUNTED") cells
                -- erase the hunted tag from the cell
                hunted' = GridCell.setTag hunted ""
            in
               randomWalk grid hunted' neighborsFn


-- Walks randomly then returns when we're stuck
randomWalk : Grid a ->
    GridCell ->
    (Grid a -> GridCell -> List GridCell) ->
    Grid a
randomWalk grid gcell neighborsFn =
    let cell = Debug.log "random walk from " <| GridCell.base gcell
    in
       if Cell.isNilCell cell
          then grid
          else
          let unvisitedNeighbors = Grid.filterNeighbors2 neighborsFn (\c -> not <| Cell.hasLinks (GridCell.base c)) grid gcell
              -- refresh cell state from grid
              gcell' = GridCell.maybeGridCellToGridCell <| Grid.getCellById grid cell.id
          in
             if isEmpty unvisitedNeighbors
                -- At a dead-end, mark the cell and return the grid
             then 
             -- first get the cell from the grid to keep state
             let deadend = GridCell.setTag gcell' "DEADEND"
             in
                Grid.updateCellById grid (Debug.log "DEADEND CELL" <| cell.id) deadend
             else
             -- random walk phase
             let neighbor = GridUtils.sampleCell unvisitedNeighbors grid.rnd
                 |> GridCell.maybeGridCellToGridCell
                 grid' = Grid.linkCells grid gcell' neighbor True
                 grid'' = Grid.updateRnd grid'
             in
                 randomWalk grid'' neighbor neighborsFn

-- Breaking out to try trampoline
work : Grid a ->
    GridCell ->
    (Grid a -> GridCell -> List GridCell) ->
    Trampoline (Grid a)
work grid gcell neighborsFn =
    let cell = GridCell.base gcell
    in
       if Cell.isNilCell cell
          then Done grid
          else
          let unvisitedNeighbors = Grid.filterNeighbors2 neighborsFn (\c -> not <| Cell.hasLinks (GridCell.base c)) grid gcell
          in
             if not <| isEmpty unvisitedNeighbors
                then
                -- random walk phase
                let neighbor = GridUtils.sampleCell unvisitedNeighbors grid.rnd
                    |> GridCell.maybeGridCellToGridCell
                    grid' = Grid.linkCells grid gcell neighbor True
                    grid'' = Grid.updateRnd grid'
                in
                   Continue (\() -> work grid'' neighbor neighborsFn)
               else
               -- hunt phase
               let (grid', current) = hunt grid neighborsFn
               in
                  Continue (\() -> work grid' current neighborsFn)


hunt : Grid a ->
     (Grid a -> GridCell -> List GridCell) ->
    (Grid a, GridCell)
hunt grid neighborsFn =
    let visitedNeighbors : GridCell -> List GridCell
        visitedNeighbors cell = Grid.filterNeighbors2 neighborsFn (\c -> Cell.hasLinks (GridCell.base c)) grid cell

        huntUnvisitedNeighbor : GridCell -> Bool
        huntUnvisitedNeighbor gcell =
            (not <| isEmpty (visitedNeighbors gcell)) && (not <| Cell.hasLinks (GridCell.toRectCell gcell))

        huntedCell = Debug.log "HUNTED CELL" <| LE.find huntUnvisitedNeighbor <| Grid.cellsList grid.cells
    in
       case huntedCell of
          -- no results, return an invalid cell
          Nothing -> (grid, (RectCellTag Cell.createNilCell))
          Just a ->
              -- mark cell as hunted
              let hunted' = GridCell.setTag a "HUNTED"
                  linked = GridUtils.sampleCell (visitedNeighbors a) grid.rnd
                         |> GridCell.maybeGridCellToGridCell
              in
                 ((Grid.linkCells (Grid.updateRnd grid) hunted' linked True), hunted')

