-- Module defining the (David Bruce) wilson's maze creation algorithm
module Wilsons where

import Grid exposing (..)
import Cell exposing (..)
import GridUtils

import Random
import List exposing (..)
import Trampoline exposing (..)
import Debug exposing (log)

type alias RandomWalkPath = {
    cell : Cell,
    path : List Cell,
    unvisited : List Cell
}

on : Grid {} -> Grid {}
on grid =
    let (grid', first) = randomCell grid
        -- get all cells but the sampled one
        firstIndex = cellIndex grid first
        unvisited = filter (\e -> e.id != first.id) grid.cells
    in
       trampoline (work grid unvisited)

work : Grid {} -> List Cell -> Trampoline (Grid {})
work grid unvisited =
    if List.isEmpty unvisited
       then Done grid
       else
       let (cell) = Cell.takeRandom unvisited
           rwp = {cell = cell, path = [cell], unvisited = unvisited}
           (grid', unvisited') = loopErasedRandomWalk grid rwp
       in
          Continue (\() -> (work grid' unvisited'))

loopErasedRandomWalk : Grid {} -> RandomWalkPath -> (Grid {}, List Cell)
loopErasedRandomWalk grid rwp =
    if not <| member rwp.cell rwp.unvisited
       then carvePassage grid rwp
       else
       let cell' = Cell.takeRandom <| neighbors grid cell
           position = indexOf cell rwp.path
       in
          if position
             then loopErasedRandomWalk grid {rwp | cell <- cell, path <- path[0..position]}
             else loopErasedRandomWalk grid {rwp | path <- concat path [cell]}
                0

indexOf : Cell -> List Cell -> Int
indexOf cell cells =
    -- is there a better way?
    let indexed = indexedMap (,) cells
        found = filter (\e -> (snd e) == cell) cells
    in
       if isEmpty found
          then -1
          else fst (head found)
