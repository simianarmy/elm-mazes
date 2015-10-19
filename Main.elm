module Main where

import String
import Random exposing (Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple as StartApp

import Grid exposing (Grid, createGrid, toTitle, nextSeed)
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

type Action = Refresh |
    UpdateWidth String |
    UpdateHeight String

update : MazeAttributes -> Action -> Model -> Model
update context action model =
    case action of
        -- TODO: Implement Grid.update, get dimensions from inputs
        Refresh -> 
            context.alg (Grid.update model)
        
        UpdateWidth str -> {
            model | rows <- String.toInt str |> Result.toMaybe |> Maybe.withDefault model.rows
        }

        UpdateHeight str -> {
            model | cols <- String.toInt str |> Result.toMaybe |> Maybe.withDefault model.cols
        }

-- VIEW
view : MazeAttributes -> Signal.Address Action -> Model -> Html
view context address model =
    div [] [
        text (toTitle model),
        Grid.view model,
        --pre [] [text (Grid.toAscii model)],
        input [ value (toString model.rows)
              , on "input" targetValue (Signal.message address << UpdateWidth) ] [],
        text " X ",
        input [ value (toString model.cols)
              , on "input" targetValue (Signal.message address << UpdateHeight)] [],
        button [ onClick address Refresh ] [ text "REFRESH" ]
        ]

port startTime : Float
startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime
-- uncomment to debug with consistent seed
--startTimeSeed = Random.initialSeed 123

main =
    let context = {
        alg = initAlg,
        width = initWidth,
        height = initHeight
    } 
    in
       StartApp.start {
           model = init context startTimeSeed
                      , update = update context
                      , view = view context
                  }

--};</script></head><body><script type="text/javascript">Elm.fullscreen(Elm.Main, {startTime: Date.now()})</script></body></html>
