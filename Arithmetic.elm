module Arithmetic exposing (isEven, isOdd)

isEven : Int -> Bool
isEven x = x % 2 == 0

isOdd : Int -> Bool
isOdd x = not <| isEven x
