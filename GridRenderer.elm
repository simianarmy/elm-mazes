module GridRenderer exposing
    ( toAscii
    , toColoredElement
    , toWeightedElement
    )

import Grid exposing (Grid)
import GridCell exposing (GridCell)
import DistanceGrid exposing (CellDistances)
import ColoredGrid
import WeightedGrid exposing (Weighted)
import Distances exposing (Distances)

import Html
import Element as GE
import Color exposing (Color, rgb)

toAscii :
    -- maze grid
    Grid a ->
    -- cell painter
    (Grid a -> GridCell -> String) ->
    -- returns
    String
toAscii grid cellPainter =
    Grid.toAscii grid (cellPainter grid)

-- We need to pass Colored but that would mean Grid would have a cyclic 
-- dependency on it, so what I need to do is probably move cellPainter to
-- ColoredGrid.
--
-- generates collage object (Element) of the grid
-- Takes 2 painter functions: one for the whole grid and one for each cell
toColoredElement :
    -- maze grid
    Grid a ->
    -- grid painter
    (Grid a -> (GridCell -> Color) -> Int -> GE.Element) ->
    -- start cell
    GridCell ->
    -- cell size
    Int ->
    -- returns
    GE.Element
toColoredElement grid gridPainter startCell cellSize =
    let coloredGrid = ColoredGrid.createGrid grid startCell
        -- curry the colored grid to the cell painter so that we can pass cellPainter function to 
        -- modules that don't know about Colored grids
        cellPainter = ColoredGrid.cellBackgroundColor coloredGrid
    in
        gridPainter grid cellPainter cellSize

toWeightedElement :
    -- maze grid
    Weighted a ->
    -- grid painter
    (Grid a -> (GridCell -> Color) -> Int -> GE.Element) ->
    -- cell size
    Int ->
    -- returns
    GE.Element
toWeightedElement wgrid gridPainter cellSize =
    let cellPainter = WeightedGrid.cellBackgroundColor wgrid
    in
        gridPainter wgrid.dgrid.grid cellPainter cellSize

