-- Hex Grid module
module HexGrid where

import Grid exposing (..)
import Mask exposing (Mask)
import Cell exposing (Cell, BaseCell, CellID, CellLinks)
import GridCell exposing (..)
import GridUtils
import Rnd
import Arithmetic

import Set
import Array exposing (Array)
import Graphics.Element as GE
import Graphics.Collage as GC
import Html
import Color exposing (Color)


makeCells : Mask -> CellGrid
makeCells mask =
    let createMaskedCell row col =
        if Mask.get mask row col
           then HexCellTag (Cell.createCell row col)
           else HexCellTag (Cell.createMaskedCell row col)

        makeRow row cols =
            Array.initialize mask.cols (\n -> createMaskedCell row n)

        acells = Array.initialize mask.rows (\n -> makeRow n mask.cols)
    in
        configureCells mask.rows mask.cols acells

-- no-op
configureCells : Int -> Int -> CellGrid -> CellGrid
configureCells rows cols incells =
    incells

-- return row index of a cell's north diagonal
northDiag : BaseCell -> Int
northDiag cell =
    if Arithmetic.isOdd cell.col
       then cell.row - 1
       else cell.row

-- return row index of a cell's south diagonal
southDiag : BaseCell -> Int
southDiag cell = 
    if Arithmetic.isOdd cell.col
       then cell.row
       else cell.row + 1

northwest : Grid a -> BaseCell -> Maybe GridCell
northwest grid cell =
    getCell grid (northDiag cell) (cell.col - 1)

north : Grid a -> BaseCell -> Maybe GridCell
north grid cell =
    getCell grid (cell.row - 1) cell.col

northeast : Grid a -> BaseCell -> Maybe GridCell
northeast grid cell =
    getCell grid (northDiag cell) (cell.col + 1)

southwest : Grid a -> BaseCell -> Maybe GridCell
southwest grid cell =
    getCell grid (southDiag cell) (cell.col - 1)

south : Grid a -> BaseCell -> Maybe GridCell
south grid cell =
    getCell grid (cell.row + 1) cell.col

southeast : Grid a -> BaseCell -> Maybe GridCell
southeast grid cell =
    getCell grid (southDiag cell) (cell.col + 1)

getCell : Grid a -> Int -> Int -> Maybe GridCell
getCell grid row col =
    Grid.getCell grid row col

neighbors : Grid a -> GridCell -> List GridCell
neighbors grid gc =
    case gc of
        HexCellTag cell ->
            GridUtils.smooshMaybes [
                northwest grid cell,
                north grid cell,
                northeast grid cell,
                southwest grid cell,
                south grid cell,
                southeast grid cell
            ]

        _ -> Debug.crash "Illegal call to HexGrid.neighbors with non-HexCellTag type cell"

