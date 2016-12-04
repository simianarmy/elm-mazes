-- Thanks Evan
-- https://gist.githubusercontent.com/evancz/954c9379f8272dfac2e7/raw/1f06a4e748cecce58b8d164349b39d8f0d1bca41/Slider.elm
module Slider exposing (Model, init, Action, update, view)

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events


-- MODEL

type alias Model =
  { id    : String
  , label : String
  , value : String
  , min   : Float
  , max   : Float
  , step  : Float
  }


init : Model -> Model
init model = model


-- UPDATE

type Action
  = Change String


update : Action -> Model -> Model
update action model =
  case action of
    Change newValue ->
      { model | value = newValue }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [
    Attributes.id model.id
  , Attributes.class "slider"
  ]
  [
    input [
      Attributes.class "slider-range"
    , Attributes.type' "range"
    , Attributes.value model.value
    , Attributes.min (toString model.min)
    , Attributes.max (toString model.max)
    , Attributes.step (toString model.step)
    , onInput address Change
    ]
    []
  , input [
      Attributes.class "slider-text"
    , Attributes.type' "number"
    , Attributes.value model.value
    , onInput address Change
    ]
    []
  ]

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput addr f =
  Events.on "input" Events.targetValue (\value -> Signal.message addr (f value))
