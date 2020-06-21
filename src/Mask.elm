-- Mask module for shaping grids


module Mask exposing (..)

import Array exposing (Array)
import Debug
import Rnd
import String


type alias Mask =
    { rows : Int
    , cols : Int
    , bits : Array (Array Bool)
    }


maxGetRandomLocationTries =
    10000


createMask : Int -> Int -> Mask
createMask cols rows =
    let
        bits =
            Array.initialize rows (\n -> Array.repeat cols True)
    in
    { rows = rows
    , cols = cols
    , bits = bits
    }



-- 0-based indices


get : Mask -> Int -> Int -> Bool
get mask row col =
    case Array.get row mask.bits of
        Just a ->
            case Array.get col a of
                Just b ->
                    b

                Nothing ->
                    False

        Nothing ->
            False



-- 0-based indices


set : Mask -> ( Int, Int ) -> Bool -> Mask
set mask ( row, col ) isOn =
    case Array.get row mask.bits of
        -- update columns array
        Just rowbits ->
            { mask
                | bits = Array.set row (Array.set col isOn rowbits) mask.bits
            }

        Nothing ->
            mask



-- set multiple values from list
-- 0-based indices


mset : Mask -> List ( ( Int, Int ), Bool ) -> Mask
mset mask vals =
    let
        setone e mask_ =
            set mask_ (Tuple.first e) (Tuple.second e)
    in
    List.foldl setone mask vals



-- counts True bits


count : Mask -> Int
count mask =
    let
        addCols rowbits =
            Array.foldl (+) 0 <|
                Array.map
                    (\b ->
                        if b then
                            1

                        else
                            0
                    )
                    rowbits
    in
    Array.foldl (+) 0 <| Array.map addCols mask.bits



-- returns 0-based (row,col) pair corresponding to a random, enabled location in the grid


randomLocation : Mask -> Rnd.GridRnd -> ( Int, Int )
randomLocation mask rnd =
    getRandomLoc mask rnd 1


getRandomLoc mask rnd itr =
    if get mask rnd.row rnd.col then
        ( rnd.row, rnd.col )

    else if itr > maxGetRandomLocationTries then
        ( 0, 0 )
        -- what wil this do??

    else
        getRandomLoc mask (Rnd.refresh rnd) (itr + 1)



-- Creates mask from text (one row per line)


fromTxt : List String -> Mask
fromTxt lines =
    let
        validLines =
            List.map String.trim <| List.filter (\l -> not <| String.isEmpty l) lines

        rows =
            List.length validLines

        cols =
            String.length <| Maybe.withDefault "" (List.head validLines)

        linesArr =
            Array.fromList validLines

        rowBools : Int -> Array Bool
        rowBools row =
            let
                rowStr =
                    Array.get row linesArr

                cols =
                    String.toList <| Maybe.withDefault "" rowStr
            in
            Array.fromList <| List.map (\c -> c /= 'X') cols
    in
    { rows = rows
    , cols = cols
    , bits = Array.initialize rows rowBools
    }



-- Creates mask from 1D array of flags


fromImage : ( Int, Int ) -> Array Bool -> Mask
fromImage ( cols, rows ) flags =
    let
        rowBools row =
            let
                start =
                    row * cols

                end =
                    start + cols
            in
            Array.slice start end flags
    in
    { rows = rows
    , cols = cols
    , bits = Array.initialize rows rowBools
    }
