module Main where

import String
import Random exposing (Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple as StartApp

import Maze exposing (..)

-- MODEL

type alias Model = Maze

initWidth = 10
initHeight = 10
initAlg = Maze.sidewinder

-- UPDATE

type Action = Refresh |
    UpdateWidth String |
    UpdateHeight String

update : Action -> Model -> Model
update action model =
    case action of
        Refresh ->
            Maze.update model

        UpdateWidth str ->
            Maze.updateSize model
            (String.toInt str |> Result.toMaybe |> Maybe.withDefault model.grid.rows)
            model.grid.cols

        UpdateHeight str ->
            Maze.updateSize model model.grid.rows
            (String.toInt str |> Result.toMaybe |> Maybe.withDefault model.grid.cols)

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
    div [] [
        header [] [ h1 [] [text "Amazeball Mazes" ]],
        Maze.view model,
        br [] [],
        input [ value (toString model.grid.rows)
              , on "input" targetValue (Signal.message address << UpdateWidth) ] [],
        text " X ",
        input [ value (toString model.grid.cols)
              , on "input" targetValue (Signal.message address << UpdateHeight)] [],
        button [ onClick address Refresh ] [ text "REFRESH" ]
        , text ("start time: " ++ (toString startTime)),
        footer [] []
        ]

port startTime : Float
startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime
-- uncomment to debug with consistent seed
--startTimeSeed = Random.initialSeed 123

main =
    StartApp.start {
        model = Maze.init initAlg initWidth initHeight startTimeSeed
                   , update = update
                   , view = view
               }

--};</script></head><body><script type="text/javascript">Elm.fullscreen(Elm.Main, {startTime: Date.now()})</script></body></html>
