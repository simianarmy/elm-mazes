-- Sidewinder.elm
-- Module definiing the maze-generating Sidewinder algorithm
--


module Sidewinder exposing (on, step)

import Cell
import Debug
import Grid exposing (Grid)
import GridCell exposing (..)
import GridUtils
import List exposing (..)
import Random exposing (..)


type alias RowState =
    { run : List GridCell, grid : Grid, stop : Bool }


on :
    (Grid -> Maybe GridCell)
    -> (Grid -> GridCell -> List GridCell)
    -> Grid
    -> Grid
on startCellFn neighborsFn grid =
    let
        -- bias is to start at the bottom left...may not matter
        --processRow : Int -> Grid -> Grid
        processRow row curGrid =
            let
                state =
                    { run = [], grid = curGrid, stop = False }
            in
            work state (Grid.rowCells curGrid row)
    in
    List.foldl processRow grid (List.reverse (List.range 0 (grid.rows - 1)))



-- Processes a single cell (using single 1-based index for lookup)
-- step value shouldn't care about shape of the grid


step :
    (Grid -> Maybe GridCell)
    -> (Grid -> GridCell -> List GridCell)
    -> Grid
    -> Int
    -> Grid
step startCellFn neighborsFn grid i =
    -- get the current cell
    let
        cell =
            List.head <| List.reverse <| List.take (Debug.log "STEP" i) (Grid.cellsList grid.cells)

        -- run should be all previously visited cells connected to us
        -- we can generate the run by moving west until we hit a wall
        generateRun : List GridCell -> List GridCell
        generateRun rcells =
            let
                run =
                    List.take i rcells

                run_ =
                    Debug.log "Run1" <| List.filter (\c -> Cell.isLinked (GridCell.base c) (GridCell.maybeGridCellToCell (Grid.east grid (GridCell.base c)))) run
            in
            run_
    in
    case cell of
        Just c ->
            let
                cells =
                    List.filter (\c -> not <| (GridCell.base c).tag == "PROCESSED") <|
                        Grid.rowCells grid (grid.rows - (GridCell.base c).row - 1)

                state =
                    { run = [], grid = grid, stop = False }
            in
            work state cells

        Nothing ->
            grid



-- Will link row cells until Heads or boundary.  On heads will link a random row cell to its northern neighbor.
-- Returns after Northern link is made or eastern edge reached.
-- Makes iterative processing difficult since each 'step' can result in multiple links.


work : RowState -> List GridCell -> Grid
work state cells =
    let
        processCell : GridCell -> RowState -> RowState
        processCell ogCell rowState =
            if rowState.stop then
                rowState

            else
                -- Fetch cell from grid before processing since it_s state may have been modified
                let
                    cell =
                        maybeGridCellToGridCell <| Grid.getCell rowState.grid (GridCell.row ogCell) (GridCell.col ogCell)

                    run_ =
                        cell :: rowState.run

                    runstr =
                        GridUtils.cellsToString run_

                    basecell =
                        GridCell.base cell

                    atEasternBoundary =
                        not (GridCell.isValidCell (Grid.east rowState.grid basecell))

                    atNorthernBoundary =
                        not (GridCell.isValidCell (Grid.north rowState.grid basecell))

                    -- update grid_s rnd
                    grid_ =
                        Grid.updateRnd rowState.grid

                    shouldCloseOut =
                        atEasternBoundary || (not atNorthernBoundary && grid_.rnd.heads)
                in
                if shouldCloseOut then
                    -- get random cell from run
                    let
                        runCell =
                            GridCell.maybeGridCellToGridCell <| GridUtils.sampleCell run_ grid_.rnd

                        -- fetch cell from grid to keep up to date
                        member =
                            maybeGridCellToGridCell <| Grid.getCell grid_ (GridCell.row runCell) (GridCell.col runCell)

                        bm =
                            GridCell.base member

                        northern =
                            Grid.north grid_ bm

                        grid__ =
                            Grid.updateRnd grid_
                    in
                    if GridCell.isValidCell northern then
                        { run = []
                        , stop = True
                        , -- link cells and update the grid RND
                          grid =
                            Grid.linkCells grid__
                                (GridCell.setTag member "PROCESSED")
                                (GridCell.maybeGridCellToGridCell northern)
                                True
                        }

                    else
                        { run = []
                        , grid = grid__
                        , stop = False
                        }

                else
                    { rowState
                        | run = run_
                        , stop = False
                        , -- link cells and update the grid RND
                          grid =
                            Grid.linkCells grid_
                                (GridCell.setTag cell "PROCESSED")
                                (GridCell.setTag (GridCell.maybeGridCellToGridCell <| Grid.east grid_ basecell) "PROCESSED")
                                True
                    }
    in
    List.foldl processCell state cells
        |> .grid
