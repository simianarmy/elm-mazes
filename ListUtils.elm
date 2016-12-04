module ListUtils exposing (..)

import List
import Random
import Random.List

indicesOf : a -> List a -> List Int
indicesOf thing things =
    things
    |> List.indexedMap (,)
    |> List.filter (\(idx, item) -> item == thing)
    |> List.map Tuple.first

firstIndexOf : a -> List a -> Int
firstIndexOf thing things =
    indicesOf thing things
    |> List.minimum
    |> Maybe.withDefault -1

shuffle : List a -> Random.Seed -> (List a, Random.Seed)
shuffle l seed =
    let generator = Random.List.shuffle l
    in
       Random.step generator seed
