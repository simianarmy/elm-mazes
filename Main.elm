module Main where

import Random exposing (Seed)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)

import Grid exposing (Grid, createGrid, gridToString, toTitle)
import BinaryTree

-- MODEL

type alias Model = Grid

-- UPDATE

type Action = Refresh

type alias MazeAlgorithm = Grid -> Seed -> Grid

init : MazeAlgorithm -> Grid
init alg =
    alg (createGrid 3 3) startTimeSeed

update : Action -> Model -> Model
update action model =
    case action of
        Refresh -> init BinaryTree.on

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
        model = init BinaryTree.on
          , update = update
          , view =view
      }

--};</script></head><body><script type="text/javascript">Elm.fullscreen(Elm.Main, {startTime: Date.now()})</script></body></html>
