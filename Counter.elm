module Mazes where

import Tree exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MODEL

type alias Model = Tree


-- UPDATE

type Action = Generate

update : Action -> Model -> Model
update action model =
  case action of
    Generate -> model


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ Tree.view model ]
    , button [ onClick address Generate ] [ text "Generate" ]

