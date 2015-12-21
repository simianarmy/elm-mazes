module GridUtils where

import Rnd exposing (..)
import Cell exposing (..)

import Random
import List exposing (..)
import ListUtils
import Array

sampleCell : List Cell -> GridRnd -> Maybe Cell
sampleCell sample rnd =
    let (rand, seed) = Random.generate (Random.int 0 ((List.length sample) - 1)) rnd.seed
    in
       Array.get rand (Array.fromList sample)

-- returns 0-based index of a cell in a list or -1 if not found
indexOfCell : Cell -> List Cell -> Int
indexOfCell cell cells =
    ListUtils.firstIndexOf cell.id <| List.map .id cells
