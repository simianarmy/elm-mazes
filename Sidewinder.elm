-- Sidewinder.elm
-- Module definiing the maze-generating Sidewinder algorithm
--
module Sidewinder where

import GridUtils
import Grid exposing (Grid)
import GridCell exposing (..)

import List exposing (..)
import Random exposing (..)

type alias RowState a = {run : List GridCell, grid : Grid a}

on : (Grid a -> Maybe GridCell) -> Grid a -> Grid a
on startCellFn grid =
    let -- bias is to start at the bottom left...may not matter
        processCell : GridCell -> RowState a -> RowState a
        processCell cell rowState =
            let run' = cell :: rowState.run
                basecell = GridCell.toRectCell cell
                atEasternBoundary = not (Grid.isValidCell (Grid.east rowState.grid basecell))
                atNorthernBoundary = not (Grid.isValidCell (Grid.north rowState.grid basecell))
                -- update grid's rnd
                grid' = Grid.updateRnd rowState.grid
                shouldCloseOut = atEasternBoundary || ((not atNorthernBoundary) && grid'.rnd.heads)
            in
               if shouldCloseOut
                  then 
                  -- get random cell from run
                  let member = Grid.maybeGridCellToCell <| GridUtils.sampleCell run' grid'.rnd
                      northern = Grid.north grid' member
                      grid'' = Grid.updateRnd grid'
                  in
                     if Grid.isValidCell northern
                        then
                        {
                            run = [],
                            -- link cells and update the grid RND
                            grid = Grid.linkCells grid'' 
                                (RectCellTag member)
                                (RectCellTag (Grid.toValidCell northern))
                                True
                        } 
                        else
                        {
                            run = [],
                            grid = grid''
                        } 
                  else 
                  {
                      rowState |
                      run = run',
                      -- link cells and update the grid RND
                      grid = Grid.linkCells grid' 
                          cell
                          (RectCellTag (Grid.toValidCell (Grid.east grid' basecell)))
                          True
                  }

        -- elm told me to do this
        --processRow : Int -> Grid a -> Grid a
        processRow row curGrid =
            let state = {run = [], grid = curGrid}
            in
                List.foldl processCell state (Grid.rowCells curGrid row)
                |> .grid
    in
        List.foldl processRow grid (List.reverse [0..grid.rows-1])
