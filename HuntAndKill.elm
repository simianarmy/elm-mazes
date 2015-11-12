-- Module defining the Hunt & Kill maze creation algorithm
module HuntAndKill (on) where

import Grid exposing (Grid)
import Cell exposing (Cell)
import GridUtils

import Random
import Array
import List exposing (..)
import List.Extra as LE
import Trampoline exposing (..)
import Debug exposing (log)

on : Grid a -> Grid a
on grid =
    -- start at a random cell in the grid
    let (grid', startCell) = Grid.randomCell grid

    in
       trampoline (walkRandomly grid' startCell)

-- Breaking out to try trampoline
walkRandomly : Grid a -> Cell -> Trampoline (Grid a)
walkRandomly grid cell =
    if cell.row == -1
       then Done grid
       else
       let unvisitedNeighbors = Grid.filterNeighbors (\c -> not <| Cell.hasLinks c) grid cell
       in
          if not <| isEmpty unvisitedNeighbors
             then
             -- random walk phase
             let neighbor = GridUtils.sampleCell unvisitedNeighbors grid.rnd
                 |> Grid.toValidCell
                 grid' = Grid.linkCells grid cell neighbor True
                 grid'' = Grid.updateRnd grid'
             in
                Continue (\() -> walkRandomly grid'' neighbor)
          else
          -- hunt phase
          let (grid', current) = hunt grid
          in
             Continue (\() -> walkRandomly grid' current)


hunt : Grid a -> (Grid a, Cell)
hunt grid =
    let visitedNeighbors cell = Grid.filterNeighbors (\c -> Cell.hasLinks c) grid cell

        huntUnvisitedNeighbor : Cell -> Bool
        huntUnvisitedNeighbor cell =
            (not <| isEmpty (visitedNeighbors cell)) && (not <| Cell.hasLinks cell)

        huntedCell = LE.find huntUnvisitedNeighbor grid.cells
    in
       case huntedCell of
          -- no results, return an invalid cell
          Nothing -> (grid, (Cell.createCell -1 -1))
          Just a ->
              let linked = GridUtils.sampleCell (visitedNeighbors a) grid.rnd
                         |> Grid.toValidCell
              in
                 ((Grid.linkCells (Grid.updateRnd grid) a linked True), a)

