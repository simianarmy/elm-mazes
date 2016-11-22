-- Module defining the (David Bruce) wilson's maze creation algorithm
module Wilsons exposing (on, step) where

import Grid exposing (Grid)
import PolarGrid
import Cell exposing (BaseCell)
import GridCell exposing (..)
import GridUtils

import Random
import Array
import List exposing (..)
import Trampoline exposing (..)
import Debug exposing (log)

type alias RandomWalkPath a = {
    grid : Grid a,
    cell : GridCell,
    path : List GridCell,
    unvisited : List GridCell
}

on : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a -> Grid a
on startCellFn neighborsFn grid =
    let startCell = GridCell.maybeGridCellToGridCell <| startCellFn grid
        grid' = Grid.updateRnd grid
        -- get all cells but the sampled one
        unvisited = GridCell.filterGridCells (\e ->
            not <| e.id == (GridCell.id startCell)
        ) <| Grid.cellsList grid.cells
    in
       trampoline (work grid' unvisited neighborsFn)

-- Processes a single cell (using single 1-based index for lookup)
-- step value shouldn't care about shape of the grid
step : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a -> Int ->
     Grid a
step startCellFn neighborsFn grid i =
    let unvisited = GridCell.filterGridCells (\e -> not <| e.visited) <| Grid.cellsList grid.cells
    in
        if List.isEmpty unvisited
           then
           grid
           else
           -- first time around, mark a random cell as visited
           if i == 0
              then
              let cell = Debug.log "first cell" <| GridCell.maybeGridCellToGridCell <| GridUtils.sampleCell unvisited grid'.rnd
                  grid' = Grid.updateRnd grid
              in
                 Grid.updateCellById grid' (GridCell.id cell) (GridCell.setVisited cell)
              else
              let grid' = Grid.updateRnd grid
                  cell = GridCell.maybeGridCellToGridCell <| GridUtils.sampleCell unvisited grid'.rnd
                  --unvisited' = List.filter (\c -> not <| GridCell.id c == GridCell.id cell) unvisited
                  rwp = loopErasedRandomWalk {
                      grid = Grid.updateRnd grid',
                      cell = cell,
                      path = [cell],
                      unvisited = unvisited
                  } neighborsFn
              in
                 rwp.grid

work : Grid a -> 
    List GridCell -> 
    (Grid a -> GridCell -> List GridCell) ->
    Trampoline (Grid a)
work grid unvisited neighborsFn =
    if isEmpty unvisited
       then Done grid
       else
       let (cell) = GridCell.maybeGridCellToGridCell <| GridUtils.sampleCell unvisited grid.rnd
           rwp = loopErasedRandomWalk {
               grid = Grid.updateRnd grid,
               cell = cell,
               path = [cell],
               unvisited = unvisited
           } neighborsFn
       in
          Continue (\() -> (work rwp.grid rwp.unvisited neighborsFn))

loopErasedRandomWalk : RandomWalkPath a ->
     (Grid a -> GridCell -> List GridCell) ->
    RandomWalkPath a
loopErasedRandomWalk rwp neighborsFn =
    -- while cell is in unvisited
    if not <| member rwp.cell rwp.unvisited
       then carvePassage rwp
       else
       let gccell = GridCell.maybeGridCellToGridCell
               <| GridUtils.sampleCell (neighborsFn rwp.grid rwp.cell) rwp.grid.rnd
           position = GridUtils.indexOfCell gccell rwp.path
           grid = Grid.updateRnd rwp.grid
       in
          if position >= 0
             then
             loopErasedRandomWalk {rwp | grid = grid, cell = gccell, path = take (position + 1) rwp.path} neighborsFn
             else
             loopErasedRandomWalk {rwp | grid = grid, cell = gccell, path = List.concat [rwp.path, [gccell]]} neighborsFn

carvePassage : RandomWalkPath a -> RandomWalkPath a
carvePassage rwp =
    -- Use an array for easy indexing into the path
    let pathArr = Array.fromList rwp.path

        carve : Int -> RandomWalkPath a -> RandomWalkPath a
        carve index rwp' =
            let icell = GridCell.maybeGridCellToGridCell <| Array.get index pathArr
                icellId = GridCell.id icell
                icell' = GridCell.maybeGridCellToGridCell <| Grid.getCellById rwp'.grid icellId
                nextcell = GridCell.maybeGridCellToGridCell  <| Array.get (index + 1) pathArr
                nextcell' = GridCell.maybeGridCellToGridCell <| Grid.getCellById rwp'.grid (GridCell.id nextcell)
                grid' = Grid.linkCells rwp'.grid icell' nextcell' True
                -- delete icell from unvisited
                unvisited' = GridCell.filterGridCells (\e -> not <| e.id == icellId) rwp'.unvisited
            in
               {rwp' |
                   grid = grid',
                   unvisited = unvisited'
               }
    in
       foldl carve rwp [0..((length rwp.path) - 2)]

