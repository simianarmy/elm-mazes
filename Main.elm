module Main where

import String
import Random exposing (Seed)
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import StartApp.Simple as StartApp
--import StartApp


import Maze exposing (..)

-- MODEL

type alias Model a = Maze a

initWidth = 10
initHeight = 10

-- UPDATE

type Action = Refresh |
    UpdateWidth String |
    UpdateHeight String |
    SelectAlg String

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

        SelectAlg str ->
            Maze.update {model | alg <- Maze.algByName str}

-- VIEW
view address model =
    let
        selectEvent = Html.Events.on "change" targetValue
            (\val -> Signal.message address <| SelectAlg <| val)
        algToOptions attr =
            option [selected (attr.alg == Maze.defaultAlgorithm)] [text attr.name]
    in
    div [] [
        header [] [ h1 [] [text "Amazeball Mazes" ]],
        Maze.view model,
        Maze.viewDistances model,
        br [] [],
        input [ class "sizeInput", value (toString model.grid.rows)
              , on "input" targetValue (Signal.message address << UpdateWidth) ] [],
        text " X ",
        input [ class "sizeInput", value (toString model.grid.cols)
              , on "input" targetValue (Signal.message address << UpdateHeight)] [],
        br [] [],
        select [ selectEvent ] (List.map algToOptions Maze.algorithms),
        button [ onClick address Refresh ] [ text "REFRESH" ]
        --, text ("start time: " ++ (toString startTime)),
        , footer [] []
        ]

port startTime : Float
startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime
-- uncomment to debug with consistent seed
--startTimeSeed = Random.initialSeed 123

main =
    StartApp.start {
        model = Maze.init Maze.defaultAlgorithm initWidth initHeight startTimeSeed
                   , update = update
                   , view = view
               }

--};</script></head><body><script type="text/javascript">Elm.fullscreen(Elm.Main, {startTime: Date.now()})</script></body></html>
