module ListUtils where

import List

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

