-- Module defining the Hunt & Kill maze creation algorithm


module HuntAndKill exposing (on, step)

import Array
import Cell exposing (Cell)
import Debug exposing (log)
import Grid exposing (Grid)
import GridCell exposing (..)
import GridUtils
import List exposing (..)
import List.Extra as LE
import Random
import Trampoline as TRAMP


on :
    (Grid -> Maybe GridCell)
    -> (Grid -> GridCell -> List GridCell)
    -> Grid
    -> Grid
on startCellFn neighborsFn grid =
    let
        grid_ =
            Grid.updateRnd grid

        startCell =
            GridCell.maybeGridCellToGridCell (startCellFn grid)
    in
    TRAMP.evaluate (work grid_ startCell neighborsFn)



-- Processes a single cell (using single 1-based index for lookup)
-- step value shouldn't care about shape of the grid


step :
    (Grid -> Maybe GridCell)
    -> (Grid -> GridCell -> List GridCell)
    -> Grid
    -> Int
    -> Grid
step startCellFn neighborsFn grid i =
    -- If first time through,
    let
        cells =
            Grid.cellsList grid.cells

        visited =
            GridCell.filterGridCells (\e -> e.visited) cells
    in
    if List.isEmpty visited then
        -- pick a random starting cell
        let
            startCell =
                GridCell.maybeGridCellToGridCell <| startCellFn grid

            grid_ =
                Grid.updateRnd grid
        in
        -- and start a random walk
        randomWalk grid_ startCell neighborsFn
        -- if hunt is over, begin walk

    else if List.any (\e -> (GridCell.base e).tag == "DEADEND") cells then
        -- erase the deadend tag
        let
            grid_ =
                Grid.updateCells grid (\c -> GridCell.setTag c "")

            foo =
                Debug.log "FOUND DEADEND, TIME TO HUNT!"
        in
        Tuple.first <| hunt grid_ neighborsFn

    else
        -- get the cell that the hunt produced
        let
            hunted =
                GridCell.maybeGridCellToGridCell <| head <| GridCell.filterGridCells (\e -> e.tag == "HUNTED") cells

            -- erase the hunted tag from the cell
            hunted_ =
                GridCell.setTag hunted ""
        in
        randomWalk grid hunted_ neighborsFn



-- Walks randomly then returns when we_re stuck


randomWalk :
    Grid
    -> GridCell
    -> (Grid -> GridCell -> List GridCell)
    -> Grid
randomWalk grid gcell neighborsFn =
    let
        cell =
            Debug.log "random walk from " <| GridCell.base gcell
    in
    if Cell.isNilCell cell then
        grid

    else
        let
            unvisitedNeighbors =
                Grid.filterNeighbors2 neighborsFn (\c -> not <| Cell.hasLinks (GridCell.base c)) grid gcell

            -- refresh cell state from grid
            gcell_ =
                GridCell.maybeGridCellToGridCell <| Grid.getCellById grid cell.id
        in
        if
            isEmpty unvisitedNeighbors
            -- At a dead-end, mark the cell and return the grid
        then
            -- first get the cell from the grid to keep state
            let
                deadend =
                    GridCell.setTag gcell_ "DEADEND"
            in
            Grid.updateCellById grid (Debug.log "DEADEND CELL" <| cell.id) deadend

        else
            -- random walk phase
            let
                neighbor =
                    GridUtils.sampleCell unvisitedNeighbors grid.rnd
                        |> GridCell.maybeGridCellToGridCell

                grid_ =
                    Grid.linkCells grid gcell_ neighbor True

                grid__ =
                    Grid.updateRnd grid_
            in
            randomWalk grid__ neighbor neighborsFn



-- Breaking out to try trampoline


work :
    Grid
    -> GridCell
    -> (Grid -> GridCell -> List GridCell)
    -> TRAMP.Trampoline Grid
work grid gcell neighborsFn =
    let
        cell =
            GridCell.base gcell
    in
    if Cell.isNilCell cell then
        TRAMP.done grid

    else
        let
            unvisitedNeighbors =
                Grid.filterNeighbors2 neighborsFn (\c -> not <| Cell.hasLinks (GridCell.base c)) grid gcell
        in
        if not <| isEmpty unvisitedNeighbors then
            -- random walk phase
            let
                neighbor =
                    GridUtils.sampleCell unvisitedNeighbors grid.rnd
                        |> GridCell.maybeGridCellToGridCell

                grid_ =
                    Grid.linkCells grid gcell neighbor True

                grid__ =
                    Grid.updateRnd grid_
            in
            TRAMP.jump (\() -> work grid__ neighbor neighborsFn)

        else
            -- hunt phase
            let
                ( grid_, current ) =
                    hunt grid neighborsFn
            in
            TRAMP.jump (\() -> work grid_ current neighborsFn)


hunt :
    Grid
    -> (Grid -> GridCell -> List GridCell)
    -> ( Grid, GridCell )
hunt grid neighborsFn =
    let
        visitedNeighbors : GridCell -> List GridCell
        visitedNeighbors cell =
            Grid.filterNeighbors2 neighborsFn (\c -> Cell.hasLinks (GridCell.base c)) grid cell

        huntUnvisitedNeighbor : GridCell -> Bool
        huntUnvisitedNeighbor gcell =
            (not <| isEmpty (visitedNeighbors gcell)) && (not <| Cell.hasLinks (GridCell.toRectCell gcell))

        huntedCell =
            Debug.log "HUNTED CELL" <| LE.find huntUnvisitedNeighbor <| Grid.cellsList grid.cells
    in
    case huntedCell of
        -- no results, return an invalid cell
        Nothing ->
            ( grid, RectCellTag Cell.createNilCell )

        Just a ->
            -- mark cell as hunted
            let
                hunted_ =
                    GridCell.setTag a "HUNTED"

                linked =
                    GridUtils.sampleCell (visitedNeighbors a) grid.rnd
                        |> GridCell.maybeGridCellToGridCell
            in
            ( Grid.linkCells (Grid.updateRnd grid) hunted_ linked True, hunted_ )
