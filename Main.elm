module Main where

import String
import List
import Random exposing (Seed)
import Time exposing (Time, second)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)

import Grid exposing (..)
import Cell
import BinaryTree

-- MODEL

type alias Model = Grid

-- UPDATE

type Action = Refresh

init : Seed -> Grid
init seed =
    BinaryTree.on (Grid.createGrid 3 3) seed

update : Action -> Model -> Model
update action model =
    case action of
        Refresh -> init startTimeSeed

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

main =
    start { 
        model = init startTimeSeed
          , update = update
          , view =view
      }

--};</script></head><body><script type="text/javascript">Elm.fullscreen(Elm.Main, {startTime: Date.now()})</script></body></html>
