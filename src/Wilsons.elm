-- Module defining the (David Bruce) wilson's maze creation algorithm


module Wilsons exposing (on, step)

import Array
import Cell exposing (BaseCell)
import Debug exposing (log)
import Grid exposing (Grid)
import GridCell exposing (..)
import GridUtils
import List exposing (..)
import PolarGrid
import Random
import Trampoline exposing (Trampoline, done, evaluate, jump)


type alias RandomWalkPath =
    { grid : Grid
    , cell : GridCell
    , path : List GridCell
    , unvisited : List GridCell
    }


on :
    (Grid -> Maybe GridCell)
    -> (Grid -> GridCell -> List GridCell)
    -> Grid
    -> Grid
on startCellFn neighborsFn grid =
    let
        startCell =
            GridCell.maybeGridCellToGridCell <| startCellFn grid

        grid_ =
            Grid.updateRnd grid

        -- get all cells but the sampled one
        unvisited =
            GridCell.filterGridCells
                (\e ->
                    not <| e.id == GridCell.id startCell
                )
            <|
                Grid.cellsList grid.cells
    in
    evaluate (work grid_ unvisited neighborsFn)



-- Processes a single cell (using single 1-based index for lookup)
-- step value shouldn't care about shape of the grid


step :
    (Grid -> Maybe GridCell)
    -> (Grid -> GridCell -> List GridCell)
    -> Grid
    -> Int
    -> Grid
step startCellFn neighborsFn grid i =
    let
        unvisited =
            GridCell.filterGridCells (\e -> not <| e.visited) <| Grid.cellsList grid.cells
    in
    if List.isEmpty unvisited then
        grid

    else
    -- first time around, mark a random cell as visited
    if
        i == 0
    then
        let
            cell =
                Debug.log "first cell" <| GridCell.maybeGridCellToGridCell <| GridUtils.sampleCell unvisited grid_.rnd

            grid_ =
                Grid.updateRnd grid
        in
        Grid.updateCellById grid_ (GridCell.id cell) (GridCell.setVisited cell)

    else
        let
            grid_ =
                Grid.updateRnd grid

            cell =
                GridCell.maybeGridCellToGridCell <| GridUtils.sampleCell unvisited grid_.rnd

            --unvisited_ = List.filter (\c -> not <| GridCell.id c == GridCell.id cell) unvisited
            rwp =
                loopErasedRandomWalk
                    { grid = Grid.updateRnd grid_
                    , cell = cell
                    , path = [ cell ]
                    , unvisited = unvisited
                    }
                    neighborsFn
        in
        rwp.grid


work :
    Grid
    -> List GridCell
    -> (Grid -> GridCell -> List GridCell)
    -> Trampoline Grid
work grid unvisited neighborsFn =
    if isEmpty unvisited then
        done grid

    else
        let
            cell =
                GridCell.maybeGridCellToGridCell <| GridUtils.sampleCell unvisited grid.rnd

            rwp =
                loopErasedRandomWalk
                    { grid = Grid.updateRnd grid
                    , cell = cell
                    , path = [ cell ]
                    , unvisited = unvisited
                    }
                    neighborsFn
        in
        jump (\() -> work rwp.grid rwp.unvisited neighborsFn)


loopErasedRandomWalk :
    RandomWalkPath
    -> (Grid -> GridCell -> List GridCell)
    -> RandomWalkPath
loopErasedRandomWalk rwp neighborsFn =
    -- while cell is in unvisited
    if not <| member rwp.cell rwp.unvisited then
        carvePassage rwp

    else
        let
            gccell =
                GridCell.maybeGridCellToGridCell <|
                    GridUtils.sampleCell (neighborsFn rwp.grid rwp.cell) rwp.grid.rnd

            position =
                GridUtils.indexOfCell gccell rwp.path

            grid =
                Grid.updateRnd rwp.grid
        in
        if position >= 0 then
            loopErasedRandomWalk { rwp | grid = grid, cell = gccell, path = take (position + 1) rwp.path } neighborsFn

        else
            loopErasedRandomWalk { rwp | grid = grid, cell = gccell, path = List.concat [ rwp.path, [ gccell ] ] } neighborsFn


carvePassage : RandomWalkPath -> RandomWalkPath
carvePassage rwp =
    -- Use an array for easy indexing into the path
    let
        pathArr =
            Array.fromList rwp.path

        carve : Int -> RandomWalkPath -> RandomWalkPath
        carve index rwp_ =
            let
                icell =
                    GridCell.maybeGridCellToGridCell <| Array.get index pathArr

                icellId =
                    GridCell.id icell

                icell_ =
                    GridCell.maybeGridCellToGridCell <| Grid.getCellById rwp_.grid icellId

                nextcell =
                    GridCell.maybeGridCellToGridCell <| Array.get (index + 1) pathArr

                nextcell_ =
                    GridCell.maybeGridCellToGridCell <| Grid.getCellById rwp_.grid (GridCell.id nextcell)

                grid_ =
                    Grid.linkCells rwp_.grid icell_ nextcell_ True

                -- delete icell from unvisited
                unvisited_ =
                    GridCell.filterGridCells (\e -> not <| e.id == icellId) rwp_.unvisited
            in
            { rwp_
                | grid = grid_
                , unvisited = unvisited_
            }
    in
    foldl carve rwp (List.range 0 (length rwp.path - 2))
