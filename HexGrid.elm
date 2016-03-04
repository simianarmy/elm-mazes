-- Hex Grid module
module HexGrid where

import Grid exposing (..)
import Mask exposing (Mask)
import Cell exposing (Cell, BaseCell, CellID, CellLinks)
import GridCell exposing (..)
import GridUtils
import Rnd

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


northeast : Grid a -> BaseCell -> Maybe BaseCell
northwest : Grid a -> BaseCell -> Maybe BaseCell
southeast : Grid a -> BaseCell -> Maybe BaseCell
southwest : Grid a -> BaseCell -> Maybe BaseCell

neighbors : Grid a -> GridCell -> List GridCell
neighbors grid cell =
    List.concat [
        northwest grid cell,
        Grid.north grid cell,
        northeast grid cell,
        southwest grid cell,
        Grid.south grid cell,
        southeast grid cell
    ]

