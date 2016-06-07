-- Sidewinder.elm
-- Module definiing the maze-generating Sidewinder algorithm
--
module Sidewinder where

import Debug
import GridUtils
import Grid exposing (Grid)
import GridCell exposing (..)
import Cell

import List exposing (..)
import Random exposing (..)

type alias RowState a = {run : List GridCell, grid : Grid a, stop : Bool}

on : (Grid a -> Maybe GridCell) ->
     (Grid a -> GridCell -> List GridCell) ->
     Grid a ->
     Grid a
on startCellFn neighborsFn grid =
    let -- bias is to start at the bottom left...may not matter
        --processRow : Int -> Grid a -> Grid a
        processRow row curGrid =
            let state = {run = [], grid = curGrid, stop = False}
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
    let cell = List.head <| List.reverse <| List.take (Debug.log "STEP " i) (Grid.cellsList grid.cells)
        -- run should be all previously visited cells connected to us
        -- we can generate the run by moving west until we hit a wall
        generateRun : List GridCell -> List GridCell
        generateRun rcells =
            let run = List.take i rcells
                run' =  Debug.log "Run1" <| List.filter (\c -> Cell.isLinked (GridCell.base c) (GridCell.maybeGridCellToCell (Grid.east grid (GridCell.base c)))) run
            in
               run'
    in
       case cell of
           Just c ->
               -- FIX ME: WE DON'T HAVE A CORRECT ALGORITHM FOR PICKING UNPROCESSED CELLS YET!!!
               let cells = List.filter (\c -> not (GridCell.base c).processed)
                   <| Grid.rowCells grid (grid.rows - (GridCell.base c).row - 1)
                   state = {run = [], grid = grid, stop = False}
               in
                    work state cells

           Nothing ->
               grid

-- Will link row cells until Heads or boundary.  On heads will link a random row cell to its northern neighbor.
-- Returns after Northern link is made or eastern edge reached.
-- Makes iterative processing difficult since each 'step' can result in multiple links.
work : RowState a -> List GridCell -> Grid a
work state cells =
    let processCell : GridCell -> RowState a -> RowState a
        processCell cell rowState =
            if rowState.stop
               then rowState
               else
                let run' = cell :: rowState.run
                    runstr = Debug.log "Run: " <| GridUtils.cellsToString run'
                    basecell = GridCell.base <| Debug.log "work cell" cell
                    atEasternBoundary = Debug.log "At eastern? " <| not (GridCell.isValidCell (Grid.east rowState.grid basecell))
                    atNorthernBoundary = Debug.log "At northern? " <| not (GridCell.isValidCell (Grid.north rowState.grid basecell))
                    -- update grid's rnd
                    grid' = Grid.updateRnd rowState.grid
                    shouldCloseOut = Debug.log "Close out? " <| atEasternBoundary || ((not atNorthernBoundary) && (Debug.log "Heads: " grid'.rnd.heads))
                in
                   if shouldCloseOut
                      then 
                      -- get random cell from run
                      let member = Debug.log "random cell: " <| GridCell.maybeGridCellToGridCell <| GridUtils.sampleCell run' grid'.rnd
                          bm = GridCell.base member
                          northern = Grid.north grid' bm
                          grid'' = Grid.updateRnd grid'
                      in
                         if GridCell.isValidCell northern
                            then
                            {
                                run = [],
                                stop = True,
                                -- link cells and update the grid RND
                                grid = Grid.linkCells grid'' 
                                    (GridCell.setProcessed member)
                                    (GridCell.maybeGridCellToGridCell northern)
                                    True
                            } 
                            else
                            {
                                run = Debug.log "Invalid northern cell" [],
                                grid = grid'',
                                stop = False
                            } 
                      else 
                      {
                          rowState |
                          run = run',
                          stop = False,
                          -- link cells and update the grid RND
                          grid = Grid.linkCells grid' 
                              (GridCell.setProcessed cell)
                              (GridCell.setProcessed (GridCell.maybeGridCellToGridCell <| Grid.east grid' basecell))
                              True
                      }

    in
       List.foldl processCell state cells
       |> .grid

