-- Module defining the Aldous-Broder maze creation algorithm
module AldousBroder exposing (on, step)

import Grid exposing (Grid)
import PolarGrid
import HexGrid
import TriangleGrid
import GridCell exposing (..)
import Cell exposing (Cell)
import GridUtils

import Random
import List
import Array
import Trampoline exposing (Trampoline)
import Debug exposing (log)

on : (Grid a -> Maybe GridCell) ->
    (Grid a -> GridCell -> List GridCell) ->
    Grid a -> Grid a
on startCellFn neighborsFn grid =
    let grid_ = Grid.updateRnd grid
        startCell = GridCell.maybeGridCellToGridCell <| startCellFn grid
        gridSize = case startCell of
            PolarCellTag c -> PolarGrid.size grid_
            _ -> Grid.size grid_
    in
       Trampoline.evaluate (walkRandomly grid_ neighborsFn startCell (gridSize - 1))

-- Processes a single cell (using single 1-based index for lookup)
-- step value shouldn_t care about shape of the grid
step : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a -> Int ->
     Grid a
step startCellFn neighborsFn grid i =
    -- pick last processed cell or random starting cell
    let current = List.filter (\c -> (GridCell.base c).tag == "PROCESSING") <| Grid.cellsList grid.cells
        visited = List.filter (\c -> .visited (GridCell.base c)) <| Grid.cellsList grid.cells
        startCell = Debug.log "start" <| if List.isEmpty current
          then GridCell.maybeGridCellToGridCell <| startCellFn grid
          else GridCell.maybeGridCellToGridCell <| List.head current
        startCell_ = GridCell.setTag startCell "PROCESSING"
        gridSize = case startCell_ of
            PolarCellTag c -> PolarGrid.size grid
            _ -> Grid.size grid
        grid_ = Grid.updateRnd grid
    in
       if List.length visited == gridSize
          then grid_
          else
          work grid_ neighborsFn startCell_

work : Grid a -> 
    (Grid a -> GridCell -> List GridCell) ->
    GridCell ->
    Grid a
work grid neighborsFn cell =
    let sample = neighborsFn grid cell
        -- grid's cell list needs to be updated with the cells new processing states
        cell_ = GridCell.setTag cell "PROCESSED"
        gcneighbor = Debug.log "neighbor" <| GridCell.setTag (GridCell.maybeGridCellToGridCell <| GridUtils.sampleCell sample grid.rnd)
            "PROCESSING"
        -- basecell
        neighbor = GridCell.base gcneighbor
    in
       if Cell.hasLinks neighbor
          then 
          -- update cell and neighbor in grid
          let grid_ = Grid.updateCellById grid (GridCell.id cell_) cell_
          in
             Grid.updateCellById grid_ (GridCell.id gcneighbor) gcneighbor
          -- linkCells will save the the grid's new cell states
         else Grid.linkCells grid (Debug.log "linking" cell_) (Debug.log "to" gcneighbor) True

-- Breaking out to try trampoline
walkRandomly : Grid a -> 
    (Grid a -> GridCell -> List GridCell) ->
    GridCell -> Int -> Trampoline.Trampoline (Grid a)
walkRandomly grid neighborsFn cell unvisited =
    if unvisited == 0
       then Trampoline.done grid
       else
       -- Pick a random neighbor of cell
       let 
           -- refresh rng
           grid_ = Grid.updateRnd grid
           sample = neighborsFn grid_ cell
           -- gridcell
           gcneighbor = GridCell.maybeGridCellToGridCell <| GridUtils.sampleCell sample grid.rnd
           -- basecell
           neighbor = GridCell.base gcneighbor
       in
          -- if neighbor has no links
          if not <| Cell.hasLinks neighbor
             then
             -- link cell to neighbor and move to the neighbor
             let grid__ = Grid.linkCells grid_ cell gcneighbor True
             in
                Trampoline.jump (\() -> walkRandomly grid__ neighborsFn gcneighbor (unvisited - 1))
             else
             -- move to the neighbor w/out linking
             Trampoline.jump (\() -> walkRandomly grid_ neighborsFn gcneighbor unvisited)

