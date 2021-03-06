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
    Grid ->
    -- cell painter
    (Grid -> GridCell -> String) ->
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
    Grid ->
    -- grid painter
    (Grid -> (GridCell -> Color) -> Int -> Float -> GE.Element) ->
    -- start cell
    GridCell ->
    -- dimensions
    Int -> Float ->
    -- returns
    GE.Element
toColoredElement grid gridPainter startCell cellSize cellInset =
    let coloredGrid = ColoredGrid.createGrid grid startCell
        -- curry the colored grid to the cell painter so that we can pass cellPainter function to 
        -- modules that don't know about Colored grids
        cellPainter = ColoredGrid.cellBackgroundColor coloredGrid
    in
        gridPainter grid cellPainter cellSize cellInset

toWeightedElement :
    -- maze grid
    Weighted ->
    -- grid painter
    (Grid -> (GridCell -> Color) -> Int -> Float -> GE.Element) ->
    -- cell size & inset
    Int -> Float ->
    -- returns
    GE.Element
toWeightedElement wgrid gridPainter cellSize cellInset =
    let cellPainter = WeightedGrid.cellBackgroundColor wgrid
    in
        gridPainter wgrid.dgrid.grid cellPainter cellSize cellInset

