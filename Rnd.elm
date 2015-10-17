module Rnd where

import Random exposing (..)

type alias GridRnd = {
    seed : Seed,
    row : Int,
    col : Int,
    heads : Bool,
    rowRnd : Seed -> (Int, Seed),
    colRnd : Seed -> (Int, Seed)
}

createGridRnd : Int -> Int -> Seed -> GridRnd
createGridRnd rows cols initSeed =
    {
        seed = initSeed,
        row = 0,
        col = 0,
        heads = False,
        rowRnd = generate (int 1 rows),
        colRnd = generate (int 1 cols)
    }

-- refresh all random values
refresh : GridRnd -> GridRnd
refresh rnd =
    let (nextRow, seed2) = rnd.rowRnd rnd.seed
        (nextCol, seed3) = rnd.colRnd seed2
        (headOrTail, seed4) = generate (int 1 2) seed3
    in
       { rnd |
        seed <- seed4,
        row <- nextRow,
        col <- nextCol,
        heads <- headOrTail == 1
    }

refreshCoinFlip : GridRnd -> GridRnd
refreshCoinFlip rnd =
    let (headOrTail, seed') = generate (int 1 2) rnd.seed
    in
       { rnd |
        seed <- seed',
        heads <- headOrTail == 1
    }

refreshRow : GridRnd -> GridRnd
refreshRow rnd =
    let (nextRow, seed') = rnd.rowRnd rnd.seed
    in
       { rnd |
        seed <- seed',
        row <- nextRow
    }

refreshCol : GridRnd -> GridRnd
refreshCol rnd =
    let (nextCol, seed') = rnd.colRnd rnd.seed
    in
       { rnd |
        seed <- seed',
        col <- nextCol
    }
