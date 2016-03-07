module Arithmetic where

isEven : Int -> Bool
isEven x = x % 2 == 0

isOdd x = not <| isEven x
