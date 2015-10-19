module Main where

import Random exposing (Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp

import Grid exposing (Grid, createGrid, gridToString, toTitle, nextSeed)
import BinaryTree
import Sidewinder

-- MODEL

type alias Model = Grid
type alias MazeAlgorithm = Grid -> Grid
type alias MazeAttributes = {
    alg : MazeAlgorithm,
    width : Int,
    height : Int
}

initWidth = 20 
initHeight = 20 
initAlg = Sidewinder.on

init : MazeAttributes -> Seed -> Grid
init attr seed =
    attr.alg (createGrid attr.width attr.height seed)

-- UPDATE

type Action = Refresh

update : Action -> Model -> Model
update action model =
    case action of
        -- TODO: Implement Grid.update, get dimensions from inputs
        Refresh -> 
            initAlg (Grid.update model)

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
    div [] [
        text (toTitle model),
        pre [] [text (gridToString model)],
        input [ value (toString model.rows) ] [],
        text " X ",
        input [ value (toString model.cols) ] [],
        button [ onClick address Refresh ] [ text "REFRESH" ]
        ]

port startTime : Float
startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime
-- uncomment to debug with consistent seed
--startTimeSeed = Random.initialSeed 123

main =
    StartApp.start {
        model = init {
            alg = initAlg,
            width = initWidth,
            height = initHeight
            } startTimeSeed
          , update = update
          , view =view
      }

--};</script></head><body><script type="text/javascript">Elm.fullscreen(Elm.Main, {startTime: Date.now()})</script></body></html>
