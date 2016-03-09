module TriangleGrid where

import Grid exposing (..)
import Mask exposing (Mask)
import Cell exposing (Cell, BaseCell, CellID, CellLinks)
import GridCell exposing (..)
import GridUtils
import Arithmetic

import Array exposing (Array)
import Graphics.Element as GE
import Graphics.Collage as GC
import Html
import Color exposing (Color)

makeCells : Mask -> CellGrid
makeCells mask =
    let createMaskedCell row col =
        if Mask.get mask row col
           then TriangleCellTag (Cell.createCell row col)
           else TriangleCellTag (Cell.createMaskedCell row col)

        makeRow row cols =
            Array.initialize mask.cols (\n -> createMaskedCell row n)

        acells = Array.initialize mask.rows (\n -> makeRow n mask.cols)
    in
        configureCells mask.rows mask.cols acells

-- no-op
configureCells : Int -> Int -> CellGrid -> CellGrid
configureCells rows cols incells =
    incells

upright : BaseCell -> Bool
upright {row, col} =
    Arithmetic.isEven (row + col)

north : Grid a -> BaseCell -> Maybe GridCell
north grid cell =
    if upright cell
       then Nothing
       else getCell grid (cell.row - 1) cell.col

south : Grid a -> BaseCell -> Maybe GridCell
south grid cell =
    if upright cell
       then getCell grid (cell.row + 1) cell.col
       else Nothing

east : Grid a -> BaseCell -> Maybe GridCell
east grid cell =
    Grid.east grid cell

west : Grid a -> BaseCell -> Maybe GridCell
west grid cell =
    Grid.west grid cell

neighbors : Grid a -> GridCell -> List GridCell
neighbors grid gc =
    case gc of
        TriangleCellTag cell ->
            GridUtils.smooshMaybes [
                north grid cell,
                south grid cell,
                west grid cell,
                east grid cell
            ]

        _ -> Debug.crash "Illegal call to HexGrid.neighbors with non-TriangleCellTag type cell"
