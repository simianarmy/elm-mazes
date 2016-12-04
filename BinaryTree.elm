-- BinaryTree.elm
-- Module defining the maze Binary Tree maze creation algorithm

module BinaryTree exposing (on, step)

import List exposing (..)
import Random exposing (..)
import GridUtils

import Grid exposing (..)
import GridCell exposing (..)

-- Processes entire grid
on : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a -> Grid a
on startCellFn neighborsFn grid =
    work startCellFn neighborsFn grid (Grid.cellsList grid.cells)

-- Processes a single cell (using single 1-based index for lookup)
-- step value shouldn't care about shape of the grid
step : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a -> Int ->
     Grid a
step startCellFn neighborsFn grid i =
    let cell = List.head <| List.reverse <| List.take i (Grid.cellsList grid.cells)
    in
       case cell of
           Just c ->
               work startCellFn neighborsFn grid [c]
           Nothing ->
               grid

work : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a ->
     List GridCell ->
     Grid a
work startCellFn neighborsFn grid cells =
    let getRandomNeighbor : Grid a -> GridCell -> Maybe GridCell
        getRandomNeighbor grid' cell =
            let acell = GridCell.base cell
                gcneighbors = GridUtils.smooshMaybes [
                    north grid' acell,
                    east grid' acell
                    ]
            in
               -- TODO: Maybe.andThen here?
               if isEmpty gcneighbors
                  then Nothing
                  else
                  case (GridUtils.sampleCell gcneighbors grid'.rnd) of
                      Nothing -> Nothing
                      Just c -> Just c

        processCell : GridCell -> Grid a -> Grid a
        processCell cell grid =
            let neighbor = getRandomNeighbor grid cell
                grid' = updateRnd grid
            in
               case neighbor of
                   Nothing -> grid'
                   Just neighbor -> linkCells grid' cell neighbor True
    in
       -- We want to somehow map over each cell while keeping the linking states
        List.foldl processCell grid cells
