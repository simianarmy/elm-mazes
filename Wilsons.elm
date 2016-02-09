-- Module defining the (David Bruce) wilson's maze creation algorithm
module Wilsons (on) where

import Grid exposing (Grid)
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

on : (Grid a -> Maybe GridCell) -> Grid a -> Grid a
on startCellFn grid =
    let startCell = Grid.maybeGridCellToGridCell <| startCellFn grid
        grid' = Grid.updateRnd grid
        -- get all cells but the sampled one
        unvisited = filterGridCells (\e ->
            not <| e.id == (GridCell.id startCell)
        ) grid.cells
    in
       trampoline (work grid' unvisited)

work : Grid a -> List GridCell -> Trampoline (Grid a)
work grid unvisited =
    if isEmpty unvisited
       then Done grid
       else
       let (cell) = Grid.maybeGridCellToGridCell <| GridUtils.sampleCell unvisited grid.rnd
           rwp = loopErasedRandomWalk {
               grid = Grid.updateRnd grid,
               cell = cell,
               path = [cell],
               unvisited = unvisited
           }
       in
          Continue (\() -> (work rwp.grid rwp.unvisited))

loopErasedRandomWalk : RandomWalkPath a -> RandomWalkPath a
loopErasedRandomWalk rwp =
    -- while cell is in unvisited
    if not <| member rwp.cell rwp.unvisited
       then carvePassage rwp
       else
       let gccell = Grid.maybeGridCellToGridCell
           <| GridUtils.sampleCell (Grid.neighbors rwp.grid rwp.cell) rwp.grid.rnd
           position = GridUtils.indexOfCell gccell rwp.path
           grid = Grid.updateRnd rwp.grid
       in
          if position >= 0
             then
             loopErasedRandomWalk {rwp | grid = grid, cell = gccell, path = take (position + 1) rwp.path}
             else
             loopErasedRandomWalk {rwp | grid = grid, cell = gccell, path = List.concat [rwp.path, [gccell]]}

carvePassage : RandomWalkPath a -> RandomWalkPath a
carvePassage rwp =
    -- Use an array for easy indexing into the path
    let pathArr = Array.fromList rwp.path

        carve : Int -> RandomWalkPath a -> RandomWalkPath a
        carve index rwp =
            let icell = Grid.maybeGridCellToGridCell <| Array.get index pathArr
                nextcell = Grid.maybeGridCellToGridCell  <| Array.get (index + 1) pathArr
                grid' = Grid.linkCells rwp.grid icell nextcell True
                icellId = GridCell.id icell
                -- delete icell from unvisited
                unvisited' = filterGridCells (\e -> not <| e.id == icellId) rwp.unvisited
            in
               {rwp |
                   grid = grid',
                   unvisited = unvisited'
               }
    in
       foldl carve rwp [0..((length rwp.path) - 2)]

-- Helper to apply filter to list of gridcells
filterGridCells : (BaseCell -> Bool) -> List GridCell -> List GridCell
filterGridCells fn cells =
    let filterFn = (\e ->
        let cell = case e of 
            RectCellTag c -> c
            PolarCellTag (c, _) -> c
        in
           fn cell)
    in
        List.filter filterFn cells

