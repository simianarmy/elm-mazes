-- Mask module for shaping grids
module Mask where

import Rnd

import Array exposing (Array)
import String

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

-- 1-based indices
get : Mask -> Int -> Int -> Bool
get mask row col =
    case Array.get (row - 1) mask.bits of
        Just a ->
            case Array.get (col - 1) a of
                Just b -> b
                Nothing -> False
        Nothing -> False

-- 1-based indices
set : Mask -> (Int, Int) -> Bool -> Mask
set mask (row, col) isOn =
    case Array.get (row - 1) mask.bits of
        -- update columns array
        Just rowbits -> {
           mask | bits = Array.set (row - 1) (Array.set (col - 1) isOn rowbits) mask.bits
        }
        Nothing -> mask

-- set multiple values from list
-- 1-based indices
mset : Mask -> List ((Int, Int), Bool) -> Mask
mset mask vals =
    let setone e mask' =
        set mask' (fst e) (snd e)
    in
        List.foldl setone mask vals

-- counts True bits
count : Mask -> Int
count mask =
    let addCols rowbits =
        Array.foldl (+) 0 <| Array.map (\b -> if b then 1 else 0) rowbits
    in
        Array.foldl (+) 0 <| Array.map addCols mask.bits

-- returns 1-based (row,col) pair corresponding to a random, enabled location in the grid
randomLocation : Mask -> Rnd.GridRnd -> (Int, Int)
randomLocation mask rnd =
    if get mask rnd.row rnd.col
       then (rnd.row, rnd.col)
       else randomLocation mask (Rnd.refresh rnd)

-- Creates mask from text (one row per line)
fromTxt : List String -> Mask
fromTxt lines =
    let validLines = List.map String.trim <| List.filter (\l -> not <| String.isEmpty l) lines
        rows = List.length validLines
        cols = String.length <| Maybe.withDefault "" (List.head validLines)
        linesArr = Array.fromList validLines
    
        rowBools : Int -> Array Bool
        rowBools row =
            let rowStr = Array.get row linesArr
                cols = String.toList <| Maybe.withDefault "" rowStr
            in
                Array.fromList <| List.map (\c -> c /= 'X') cols

       in
          {
              rows = rows,
              cols = cols,
              bits = Array.initialize rows rowBools
          }

