module RandomExtras exposing (Generator, generate, permutation) where

import Random.PCG as Random

{-| Must redefine Random.Generator type because Random.Generator(Generator)
is not exposed.
-}
type Generator a =
  Generator (Random.Seed -> (a, Random.Seed))


{-| Must also redefine Random.generate to fit new Generator type.
-}  
generate : Generator a -> Random.Seed -> (a, Random.Seed)
generate (Generator generate) seed =
  generate seed


{-| Generate random permutations of a given list. For example, this can 
be used to shuffle a deck of cards or select the starting positions of
tiles in a sliding puzzle.
-}
permutation : List a -> Generator (List a)
permutation list =
  let
    length = List.length list      
      
    {- Knuth shuffle subproblem -}
    randomMove n ((output, seed), input) =
      let
        (rand, newSeed) = 
          Random.generate (Random.int 0 (n-1)) seed
          
        {- Add the `rand`th element of the remaining list of inputs to the
        output permutation. -}
        output' =
          input
            |> List.drop rand
            |> List.take 1
            |> List.append output
            
        {- Strike the `rand`th element from the list of remaining inputs. -}
        input' =
          (List.take rand input) ++ (List.drop (rand+1) input)
      in 
        ((output', newSeed), input')
  in
    Generator <| \seed ->
      List.foldr randomMove (([], seed), list) [1..length]
        |> fst


-- -- View (example)

-- every : Time -> Signal Random.Seed
-- every interval = 
--   Signal.map (Random.initialSeed << floor) (Time.every interval)
  

-- main : Signal Element
-- main =
--   let
--     view = 
--       show << fst << (generate <| permutation [1..6])
--       -- The first digit changes slowly due to Random.int's implementation
--   in
--     Signal.map view (every <| 1/4 * Time.second)
