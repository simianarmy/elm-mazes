module ListUtils where

import List
import Random.PCG as Random exposing (Seed)
import RandomExtras

indicesOf : a -> List a -> List Int
indicesOf thing things =
    things
    |> List.indexedMap (,)
    |> List.filter (\(idx, item) -> item == thing)
    |> List.map fst

firstIndexOf : a -> List a -> Int
firstIndexOf thing things =
    indicesOf thing things
    |> List.minimum
    |> Maybe.withDefault -1

shuffle : List a -> Random.Seed -> (List a, Random.Seed)
shuffle l seed =
    let generator = RandomExtras.permutation l
    in
        RandomExtras.generate generator seed
