-- Mask module for shaping grids
module Mask where

import Rnd

import Array exposing (Array)

type alias Mask =
    {
        rows: Int,
        cols: Int,
        bits: Array (Array Bool)
    }

createMask : Int -> Int -> Mask
createMask rows cols =
    let bits = Array.initialize rows (\n -> Array.repeat cols True)
    in
       {
           rows = rows,
           cols = cols,
           bits = bits
       }

get : Mask -> Int -> Int -> Bool
get mask row col =
    case Array.get row mask.bits of
        Just a ->
            case Array.get col a of
                Just b -> b
                Nothing -> False
        Nothing -> False

set : Mask -> Int -> Int -> Bool -> Mask
set mask row col isOn =
    case Array.get row mask.bits of
        -- update columns array
        Just rowbits -> {
           mask | bits <- Array.set row (Array.set col isOn rowbits) mask.bits
        }
        Nothing -> mask

-- counts True bits
count : Mask -> Int
count mask =
    let addCols rowbits =
        Array.foldl (+) 0 <| Array.map (\b -> if b then 1 else 0) rowbits
    in
        Array.foldl (+) 0 <| Array.map addCols mask.bits

-- returns (row,col) pair corresponding to a random, enabled location in the grid
randomLocation : Mask -> Rnd.GridRnd -> (Int, Int)
randomLocation mask rnd =
    if get mask rnd.row rnd.col
       then (rnd.row, rnd.col)
       else randomLocation mask (Rnd.refresh rnd)
