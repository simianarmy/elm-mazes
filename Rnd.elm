module Rnd where

import Random exposing (..)

type alias GridRnd = {
    seed : Seed,
    row : Int,
    col : Int,
    rowRnd : Seed -> (Int, Seed),
    colRnd : Seed -> (Int, Seed)
}

createGridRnd : Int -> Int -> Seed -> GridRnd
createGridRnd rows cols initSeed =
    {
        seed = initSeed,
        row = 0,
        col = 0,
        rowRnd = generate (int 1 rows),
        colRnd = generate (int 1 cols)
    }

refresh : GridRnd -> GridRnd
refresh rnd =
    let (nextRow, seed2) = rnd.rowRnd rnd.seed
        (nextCol, seed3) = rnd.colRnd seed2
    in
       { rnd |
        seed <- seed3,
        row <- nextRow,
        col <- nextCol
    }
