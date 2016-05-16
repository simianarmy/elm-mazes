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

on : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a -> Grid a

on startCellFn neighborsFn grid =
    let -- bias is to start at the bottom left...may not matter
        --processRow : Int -> Grid a -> Grid a
        processRow row curGrid =
            let state = {run = [], grid = curGrid}
            in
               work state (Grid.rowCells curGrid row)
    in
        List.foldl processRow grid (List.reverse [0..grid.rows-1])

-- Processes a single cell (using single 1-based index for lookup)
-- step value shouldn't care about shape of the grid
step : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a -> Int ->
     Grid a
step startCellFn neighborsFn grid i =
    -- get the current cell
    let cell = Debug.log "cell: " <| List.head <| List.reverse <| List.take i (Grid.cellsList grid.cells)
    in
       case cell of
           Just c ->
               let cells = Grid.rowCells grid (GridCell.base c).row
                   -- TODO:
                   -- generateRun =
                   -- run should be all previously visited cells connected to us
                   -- we can generate the run by moving west until we hit a wall
                   state = {run = generateRun c, grid = grid}
               in
                  -- if at beginning of row, easy
                  -- otherwise recreate 'run' list by adding each row's cells from left to right until we're at the current column index
                  work state cells

           Nothing ->
               grid

work : RowState a -> List GridCell -> Grid a
work state cells =
    let processCell : GridCell -> RowState a -> RowState a
        processCell cell rowState =
            let run' = cell :: rowState.run
                basecell = GridCell.base cell
                atEasternBoundary = not (GridCell.isValidCell (Grid.east rowState.grid basecell))
                atNorthernBoundary = not (GridCell.isValidCell (Grid.north rowState.grid basecell))
                -- update grid's rnd
                grid' = Grid.updateRnd rowState.grid
                shouldCloseOut = atEasternBoundary || ((not atNorthernBoundary) && grid'.rnd.heads)
            in
               if shouldCloseOut
                  then 
                  -- get random cell from run
                  let member = GridCell.maybeGridCellToCell <| GridUtils.sampleCell run' grid'.rnd
                      northern = Grid.north grid' member
                      grid'' = Grid.updateRnd grid'
                  in
                     if GridCell.isValidCell northern
                        then
                        {
                            run = [],
                            -- link cells and update the grid RND
                            grid = Grid.linkCells grid'' 
                                (RectCellTag member)
                                (GridCell.maybeGridCellToGridCell northern)
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
                          (GridCell.maybeGridCellToGridCell <| Grid.east grid' basecell)
                          True
                  }

    in
       List.foldl processCell state cells
       |> .grid

