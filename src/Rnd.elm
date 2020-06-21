module Rnd exposing (..)

import Random exposing (Generator, Seed, step)


type Flip
    = Heads
    | Tails


type alias GridRnd =
    { seed : Seed
    , row : Int
    , col : Int
    , heads : Bool
    , rowRnd : Generator Int
    , colRnd : Generator Int
    }


createGridRnd : Int -> Int -> Seed -> GridRnd
createGridRnd rows cols initSeed =
    { seed = initSeed
    , row = -1
    , col = -1
    , heads = False
    , rowRnd = Random.int 0 (rows - 1)
    , colRnd = Random.int 0 (cols - 1)
    }



-- random number helpers


nextSeed : GridRnd -> Seed
nextSeed rnd =
    (refresh rnd).seed


randInt : GridRnd -> Int -> Int
randInt rnd max =
    -- dynamic generator based on input ceiling
    Tuple.first <| Random.step (Random.int 0 (max - 1)) rnd.seed



-- refresh all random values


refresh : GridRnd -> GridRnd
refresh rnd =
    let
        ( nextRow, seed2 ) =
            step rnd.rowRnd rnd.seed

        ( nextCol, seed3 ) =
            step rnd.colRnd seed2

        ( headOrTail, seed4 ) =
            step Random.bool seed3
    in
    { rnd
        | seed = seed4
        , row = nextRow
        , col = nextCol
        , heads = headOrTail
    }


refreshCoinFlip : GridRnd -> GridRnd
refreshCoinFlip rnd =
    let
        ( headOrTail, seed_ ) =
            step Random.bool rnd.seed
    in
    { rnd
        | seed = seed_
        , heads = headOrTail
    }


refreshRow : GridRnd -> GridRnd
refreshRow rnd =
    let
        ( nextRow, seed_ ) =
            step rnd.rowRnd rnd.seed
    in
    { rnd
        | seed = seed_
        , row = nextRow
    }


refreshCol : GridRnd -> GridRnd
refreshCol rnd =
    let
        ( nextCol, seed_ ) =
            step rnd.colRnd rnd.seed
    in
    { rnd
        | seed = seed_
        , col = nextCol
    }
