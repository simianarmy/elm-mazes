module GridRenderer where

import Grid exposing (Grid)
import GridCell exposing (GridCell)
import DistanceGrid exposing (CellDistances)
import ColoredGrid exposing (Colored)

import Html
import Graphics.Element as GE
import Color exposing (Color, rgb)

toAscii :
    -- maze grid
    Grid a ->
    -- grid drawer
    Grid a -> GridCell -> String
    -- start cell
    GridCell ->
    -- cell renderer
    (CellDistances a -> GridCell -> String) ->
    -- returns
    String
toAscii grid gridPainter startCell cellPainter =
    let dg = DistanceGrid.createGrid grid startCell
        cellPainter' = cellPainter dg
    in
       gridPainter grid cellPainter'

-- We need to pass Colored but that would mean Grid would have a cyclic 
-- dependency on it, so what I need to do is probably move cellPainter to
-- ColoredGrid.
--
-- generates collage object (Element) of the grid
-- Takes 2 painter functions: one for the whole grid and one for each cell
toElement :
    -- maze grid
    Grid a ->
    -- grid painter
    (Grid a -> (GridCell -> Color) -> Int -> GE.Element) ->
    -- start cell
    GridCell ->
    -- cell painter
    (Colored a -> GridCell -> Color) -> 
    -- cell size
    Int ->
    -- returns
    GE.Element
toElement grid gridPainter startCell cellPainter cellSize =
    let coloredGrid = ColoredGrid.createGrid grid startCell
        -- curry the colored grid to the cell painter so that we can pass cellPainter function to 
        -- modules that don't know about Colored grids
        cellPainter' = cellPainter coloredGrid
    in
        gridPainter grid cellPainter' cellSize

