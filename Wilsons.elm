-- Module defining the (David Bruce) wilson's maze creation algorithm
module Wilsons (on) where

import Grid exposing (Grid)
import Cell exposing (Cell)
import GridUtils

import Random
import Array
import List exposing (..)
import Trampoline exposing (..)
import Debug exposing (log)

type alias RandomWalkPath a = {
    grid : Grid a,
    cell : Cell,
    path : List Cell,
    unvisited : List Cell
}

on : (Grid a -> Cell) -> Grid a -> Grid a
on startCellFn grid =
    let startCell = startCellFn grid
        grid' = Grid.updateRnd grid
        -- get all cells but the sampled one
        unvisited = filter (\e -> not <| e.id == startCell.id) grid.cells
    in
       trampoline (work grid' unvisited)

work : Grid a -> List Cell -> Trampoline (Grid a)
work grid unvisited =
    if isEmpty unvisited
       then Done grid
       else
       let (cell) = Grid.toValidCell <| GridUtils.sampleCell unvisited grid.rnd
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
       let cell' = Grid.toValidCell
           <| GridUtils.sampleCell (Grid.neighbors rwp.grid rwp.cell) rwp.grid.rnd
           position = GridUtils.indexOfCell cell' rwp.path
           grid = Grid.updateRnd rwp.grid
       in
          if position >= 0
             then
             loopErasedRandomWalk {rwp | grid = grid, cell = cell', path = take (position + 1) rwp.path}
             else
             loopErasedRandomWalk {rwp | grid = grid, cell = cell', path = List.concat [rwp.path, [cell']]}

carvePassage : RandomWalkPath a -> RandomWalkPath a
carvePassage rwp =
    -- Use an array for easy indexing into the path
    let pathArr = Array.fromList rwp.path

        carve : Int -> RandomWalkPath a -> RandomWalkPath a
        carve index rwp =
            let icell = Grid.toValidCell <| Array.get index pathArr
                nextcell = Grid.toValidCell <| Array.get (index + 1) pathArr
                grid' = Grid.linkCells rwp.grid icell nextcell True
                -- delete icell from unvisited
                unvisited' = filter (\e -> not <| e.id == icell.id) rwp.unvisited
            in
               {rwp |
                   grid = grid',
                   unvisited = unvisited'
               }
    in
       foldl carve rwp [0..((length rwp.path) - 2)]

