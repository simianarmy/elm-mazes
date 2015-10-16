module Main where

import Random exposing (Seed)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)

import Grid exposing (Grid, createGrid, gridToString, toTitle, nextSeed)
import BinaryTree

-- MODEL

type alias Model = Grid
type alias MazeAlgorithm = Grid -> Grid

init : MazeAlgorithm -> Seed -> Grid
init alg seed =
    alg (createGrid 20 20 seed)

-- UPDATE

type Action = Refresh

update : Action -> Model -> Model
update action model =
    case action of
        Refresh -> init BinaryTree.on (nextSeed model)

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
    div [] [
        text (toTitle model),
        pre [] [text (gridToString model)],
        button [ onClick address Refresh ] [ text "REFRESH" ]
        ]

port startTime : Float
startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime
-- uncomment to debug with consistent seed
--startTimeSeed = Random.initialSeed 123

main =
    start {
        model = init BinaryTree.on startTimeSeed
          , update = update
          , view =view
      }

--};</script></head><body><script type="text/javascript">Elm.fullscreen(Elm.Main, {startTime: Date.now()})</script></body></html>
