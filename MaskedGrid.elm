-- Grid with mask
module MaskedGrid where

import Mask exposing (Mask)
import Cell exposing (Cell)
import Grid exposing (Grid)

import Random exposing (Seed)
import Debug

type alias Masked a = {
    a|
        mask : Mask
    }

--createGrid : Mask -> Seed -> Masked (Grid a)
createGrid mask initSeed =
    let grid = Grid.createGrid mask.rows mask.cols initSeed
    in
       {
           rows = grid.rows,
           cols = grid.cols,
           cells = prepareGrid mask,
           rnd = grid.rnd,
           mask = mask,
           -- WELP, NOW I HAVE TO ADD OTHER TYPES' PROPS :(
           maximum = 0,
           dists = []
       }

prepareGrid : Mask -> List Cell
prepareGrid mask =
    let createMaskedCell row col =
        if Mask.get mask row col
           then Cell.createCell row col
           else Cell.createMaskedCell row col

        makeRow cols row =
            List.map (createMaskedCell row) [1..(mask.cols)]
    in
       List.concatMap (makeRow mask.cols) [1..(mask.rows)]

update grid =
    {grid |
        cells = Debug.log "updated cells: " prepareGrid grid.mask
    }

randomCell : Masked (Grid a) -> Cell
randomCell grid =
    let (row, col) = Mask.randomLocation grid.mask grid.rnd
    in
       Grid.getCell grid row col |> Grid.toValidCell

size : Masked (Grid a) -> Int
size grid =
    Mask.count grid.mask
