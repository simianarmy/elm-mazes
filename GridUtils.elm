module GridUtils where

import Rnd exposing (..)
import Cell exposing (..)

import Random
import List
import Array

sampleCell : List Cell -> GridRnd -> Maybe Cell
sampleCell sample rnd =
    let (rand, seed) = Random.generate (Random.int 0 ((List.length sample) - 1)) rnd.seed
    in
       Array.get rand (Array.fromList sample)
