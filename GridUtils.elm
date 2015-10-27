module GridUtils where

import Rnd exposing (..)
import Cell exposing (..)

import Random
import List exposing (..)
import Array

sampleCell : List Cell -> GridRnd -> Maybe Cell
sampleCell sample rnd =
    let (rand, seed) = Random.generate (Random.int 0 ((List.length sample) - 1)) rnd.seed
    in
       Array.get rand (Array.fromList sample)

-- returns 0-based index of a cell in a list or -1 if not found
indexOfCell : Cell -> List Cell -> Int
indexOfCell cell cells =
    -- is there a better way?
    let indexed = indexedMap (,) cells
        found = filter (\e -> (snd e).id == cell.id) indexed
        notFound = -1
    in
       case head found of
           Just a -> fst a
           _ -> notFound
