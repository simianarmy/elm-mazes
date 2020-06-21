-- Thanks Evan
-- https://gist.githubusercontent.com/evancz/954c9379f8272dfac2e7/raw/1f06a4e748cecce58b8d164349b39d8f0d1bca41/Slider.elm


module Slider exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events
import Platform.Cmd as Cmd exposing (Cmd)



-- MODEL


type alias Model =
    { id : String
    , label : String
    , value : String
    , min : Float
    , max : Float
    , step : Float
    }


init : Model -> Model
init model =
    model



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update (Change v) model =
    { model | value = v }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Attributes.id model.id
        , Attributes.class "slider"
        ]
        [ input
            [ Attributes.class "slider-range"
            , Attributes.type_ "range"
            , Attributes.value model.value
            , Attributes.min (toString model.min)
            , Attributes.max (toString model.max)
            , Attributes.step (toString model.step)
            , Events.onInput Change
            ]
            []
        , input
            [ Attributes.class "slider-text"
            , Attributes.type_ "number"
            , Attributes.value model.value
            , Events.onInput Change
            ]
            []
        ]
