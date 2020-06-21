module Arithmetic exposing (isEven, isOdd)


isEven : Int -> Bool
isEven x =
    modBy 2 x == 0


isOdd : Int -> Bool
isOdd x =
    not <| isEven x
