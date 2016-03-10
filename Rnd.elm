module Rnd where

import Random.PCG as Random exposing (..)

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
        row = -1,
        col = -1,
        heads = False,
        rowRnd = generate (int 0 (rows-1)),
        colRnd = generate (int 0 (cols-1))
    }


-- random number helpers
nextSeed : GridRnd -> Seed
nextSeed rnd =
    (refresh rnd).seed

randInt : GridRnd -> Int -> Int
randInt rnd max =
    -- dynamic generator based on input ceiling
    fst <| generate (int 0 (max-1)) rnd.seed

-- refresh all random values
refresh : GridRnd -> GridRnd
refresh rnd =
    let (nextRow, seed2) = rnd.rowRnd rnd.seed
        (nextCol, seed3) = rnd.colRnd seed2
        (headOrTail, seed4) = generate (int 1 2) seed3
    in
       { rnd |
        seed = seed4,
        row = nextRow,
        col = nextCol,
        heads = headOrTail == 1
    }

refreshCoinFlip : GridRnd -> GridRnd
refreshCoinFlip rnd =
    let (headOrTail, seed') = generate (int 1 2) rnd.seed
    in
       { rnd |
        seed = seed',
        heads = headOrTail == 1
    }

refreshRow : GridRnd -> GridRnd
refreshRow rnd =
    let (nextRow, seed') = rnd.rowRnd rnd.seed
    in
       { rnd |
        seed = seed',
        row = nextRow
    }

refreshCol : GridRnd -> GridRnd
refreshCol rnd =
    let (nextCol, seed') = rnd.colRnd rnd.seed
    in
       { rnd |
        seed = seed',
        col = nextCol
    }
