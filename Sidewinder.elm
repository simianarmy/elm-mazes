-- Sidewinder.elm
-- Moduel definiing the maze-generating Sidewinder algorithm
--
module Sidewinder where

import GridUtils
import Grid exposing (..)
import Cell exposing (Cell)

import List exposing (..)
import Random exposing (..)

type alias RowState a = {run : List Cell, grid : Grid a}

on : (Grid a -> Cell) -> Grid a -> Grid a
on startCellFn grid =
    let -- bias is to start at the bottom left...may not matter
        bottomLeftToTopRightCells = List.concatMap (rowCells grid) (List.reverse [1..grid.rows])

        processCell : Cell -> RowState a -> RowState a
        processCell cell rowState =
            let run' = cell :: rowState.run
                atEasternBoundary = not (isValidCell (east rowState.grid cell))
                atNorthernBoundary = not (isValidCell (north rowState.grid cell))
                -- update grid's rnd
                grid' = updateRnd rowState.grid
                shouldCloseOut = atEasternBoundary || ((not atNorthernBoundary) && grid'.rnd.heads)
            in
               if shouldCloseOut
                  then 
                  -- get random cell from run
                  let member = toValidCell <| GridUtils.sampleCell run' grid'.rnd
                      northern = north grid' member
                      grid'' = updateRnd grid'
                  in
                     if isValidCell northern
                        then
                        {
                            run = [],
                            -- link cells and update the grid RND
                            grid = linkCells grid'' member (toValidCell northern) True
                        } 
                        else
                        {
                            run = [],
                            grid = grid''
                        } 
                  else 
                  {
                      rowState |
                      run <- run',
                      -- link cells and update the grid RND
                      grid <- linkCells grid' cell (toValidCell (east grid' cell)) True
                  }

        -- elm told me to do this
        --processRow : Int -> Grid a -> Grid a
        processRow row curGrid =
            let state = {run = [], grid = curGrid}
                result = List.foldl processCell state (rowCells curGrid row)
            in
               result.grid
    in
        List.foldl processRow grid (List.reverse [1..grid.rows])
