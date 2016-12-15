-- BinaryTree.elm
-- Module defining the maze Binary Tree maze creation algorithm

module BinaryTree exposing (on, step)

import List exposing (..)
import Random exposing (..)
import GridUtils

import Grid exposing (..)
import GridCell exposing (..)

-- Processes entire grid
on : (Grid -> Maybe GridCell) ->
     (Grid -> GridCell -> List GridCell) ->
     Grid -> Grid
on startCellFn neighborsFn grid =
    work startCellFn neighborsFn grid (Grid.cellsList grid.cells)

-- Processes a single cell (using single 1-based index for lookup)
-- step value shouldn't care about shape of the grid
step : (Grid -> Maybe GridCell) ->
     (Grid -> GridCell -> List GridCell) ->
     Grid -> Int ->
     Grid
step startCellFn neighborsFn grid i =
    let cell = List.head <| List.reverse <| List.take i (Grid.cellsList grid.cells)
    in
       case cell of
           Just c ->
               work startCellFn neighborsFn grid [c]
           Nothing ->
               grid

work : (Grid -> Maybe GridCell) ->
     (Grid -> GridCell -> List GridCell) ->
     Grid ->
     List GridCell ->
     Grid
work startCellFn neighborsFn grid cells =
    let getRandomNeighbor : Grid -> GridCell -> Maybe GridCell
        getRandomNeighbor grid_ cell =
            let acell = GridCell.base cell
                gcneighbors = GridUtils.smooshMaybes [
                    north grid_ acell,
                    east grid_ acell
                    ]
            in
               -- TODO: Maybe.andThen here?
               if isEmpty gcneighbors
                  then Nothing
                  else
                  case (GridUtils.sampleCell gcneighbors grid_.rnd) of
                      Nothing -> Nothing
                      Just c -> Just c

        processCell : GridCell -> Grid -> Grid
        processCell cell grid =
            let neighbor = getRandomNeighbor grid cell
                grid_ = updateRnd grid
            in
               case neighbor of
                   Nothing -> grid_
                   Just neighbor -> linkCells grid_ cell neighbor True
    in
       -- We want to somehow map over each cell while keeping the linking states
        List.foldl processCell grid cells
