-- Module defining the Recursive Backtracker maze creation algorithm
module RecursiveBacktracker exposing (on, step)

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
import Trampoline exposing (Trampoline, evaluate, done, jump)
import Debug exposing (log)

on : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a ->
     Grid a
on startCellFn neighborsFn grid =
    let grid_ = Grid.updateRnd grid
        gcell = GridCell.maybeGridCellToGridCell <| startCellFn grid
    in
       evaluate (walkRandomly grid_ neighborsFn [gcell])

-- Processes a single cell (using single 1-based index for lookup)
-- step value shouldn't care about shape of the grid
step : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a -> Int ->
     Grid a
step startCellFn neighborsFn grid i =
    -- Find most recent cell using the tag property
    if List.isEmpty <| grid.stack
       then
       if i > 0
          then Debug.log "DONE!" grid -- we're done
          -- else start at a random cell
          else 
          let current = GridCell.maybeGridCellToGridCell <| startCellFn grid
          in
             {grid | 
             stack = (GridCell.id current) :: grid.stack
         }
         else
         let currentId = head grid.stack 
         in
            case currentId of
                Nothing -> grid
                Just cid ->
                    let currentCell = GridCell.maybeGridCellToGridCell <| Grid.getCellById grid cid
                    in work grid neighborsFn currentCell

work : Grid a -> 
    (Grid a -> GridCell -> List GridCell) ->
    GridCell ->
    Grid a
work grid neighborsFn currentCell =
    let neighbors = Grid.filterNeighbors2 neighborsFn (\c -> not <| Cell.hasLinks (GridCell.base c)) grid currentCell
    in
       -- is current cell at a dead end?
       if isEmpty neighbors
          then
          let stack_ = Maybe.withDefault [] (tail grid.stack)
          in
             {grid | stack = stack_}
          else
          -- carve a path to a random neighbor
          let neighbor = GridUtils.sampleCell neighbors grid.rnd
              |> GridCell.maybeGridCellToGridCell
              grid_ = Grid.linkCells grid currentCell neighbor True
              grid__ = Grid.updateRnd grid_
          in
              -- and make it the head of the stack
             {grid__ |
             stack = (GridCell.id neighbor) :: grid__.stack
         }

walkRandomly : Grid a ->
    (Grid a -> GridCell -> List GridCell) ->
    List GridCell ->
    Trampoline (Grid a)
walkRandomly grid neighborsFn stack =
    if isEmpty stack
       then done grid
       else
       let current = head stack |> GridCell.maybeGridCellToGridCell
           -- foo = case current of
           --     HexCellTag c -> current
           --     _ -> Debug.crash "here is the failuer"
           -- Get the proper filter function
           neighbors = Grid.filterNeighbors2 neighborsFn (\c -> not <| Cell.hasLinks (GridCell.base c)) grid current
       in
           if isEmpty neighbors
              then jump (\() -> walkRandomly grid neighborsFn (Maybe.withDefault [] (tail stack)))
              else
              let neighbor = GridUtils.sampleCell neighbors grid.rnd
                           |> GridCell.maybeGridCellToGridCell
                  grid_ = Grid.linkCells grid current neighbor True
                  grid__ = Grid.updateRnd grid_
              in
                  jump (\() -> walkRandomly grid__ neighborsFn (neighbor :: stack))

