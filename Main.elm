module Main where

import Random exposing (Seed)
import Maze exposing (..)
import Mask exposing (Mask)

import Debug
import Array exposing (Array)
import String
import Signal exposing (Signal, Address)
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)

initWidth = 4
initHeight = 4
initDisplay = Maze.Polar

--- MODEL ---

-- The full application state
type alias AppState a = 
    { maze : Maze a
    }

type alias PngData =
    { width : Int,
      height : Int,
      blackFlags : Array Bool
    }

type alias Model a = AppState a

--- UPDATE ---

-- A description of the kinds of actions that can be performed on the model of
-- our application. See the following for more info on this pattern and
-- some alternatives: https://github.com/evancz/elm-architecture-tutorial/
type Action = 
    NoOp
    | Refresh
    | UpdateWidth String
    | UpdateHeight String
    | SelectAlg String
    | SelectView Maze.Display
    | LoadAsciiMask (List String)
    | LoadImageMask PngData

-- How we update our Model on a given Action?
--update : Action -> Model a -> Model a
update action model =
    case action of
        NoOp -> model

        Refresh ->
            {model | maze = Maze.update model.maze}

        UpdateWidth str ->
            let maze' = Maze.updateSize model.maze (String.toInt str |> Result.toMaybe |> Maybe.withDefault model.maze.grid.cols) model.maze.grid.rows
            in
               {model | maze = maze'}

        UpdateHeight str ->
            let maze' = Maze.updateSize model.maze model.maze.grid.cols (String.toInt str |> Result.toMaybe |> Maybe.withDefault model.maze.grid.rows)
            in
               {model | maze = maze'}

        SelectAlg str ->
            let maze' = model.maze
                maze'' = Maze.update {maze' | alg = Maze.algByName str}
            in
               {model | maze = maze''}

        SelectView display ->
            let maze' = model.maze
                maze'' = {maze' | display = display}
            in
               {model | maze = maze''}

        LoadAsciiMask lines ->
            let mask = Mask.fromTxt <| Debug.log "lines from input file: " lines
            in
               {model | maze = Maze.setMask model.maze mask}

        LoadImageMask png ->
            let mask = Mask.fromImage (png.width, png.height) png.blackFlags
            in
               {model | maze = Maze.setMask model.maze mask}


--- VIEW ---
--view : Address Action -> Model a -> Html
view address model =
    let
        selectAlg = Html.Events.on "change" targetValue
            (\val -> Signal.message address <| SelectAlg <| val)
        selectView = Html.Events.on "change" targetValue
            (\val -> Signal.message address <| SelectView <| displayFromString val)
        algToOptions attr =
            option [selected (attr.alg == Maze.defaultAlgorithm)] [text attr.name]
        viewOptions = [
            option [selected True] [text "Ascii"]
            , option [] [text "Colored"]
            , option [] [text "Polar"]
            ]
        maze = model.maze
    in
    div [] [
        -- title
        header [] [ h1 [] [text "Amazeball Mazes" ]]
        -- the maze
        , Maze.view maze
        -- controls
        , text "width X height"
        , br [] []
        , input [ class "sizeInput", value (toString maze.grid.cols)
              , on "input" targetValue (Signal.message address << UpdateWidth) ] []
        , text " X "
        , input [ class "sizeInput", value (toString maze.grid.rows)
              , on "input" targetValue (Signal.message address << UpdateHeight)] []
        , br [] []
        , select [ selectAlg ] (List.map algToOptions <| Maze.algorithms maze.display)
        , select [ selectView ] (viewOptions)
        , button [ onClick address Refresh ] [ text "REFRESH" ]
        , br [] []
        , text "Ascii Mask file: "
        , input [ type' "file", id "maskfileinput" ] []
        --, text ("start time: " ++ (toString startTime)),
        , footer [] []
        ]

displayFromString : String -> Display
displayFromString str =
    if str == "Ascii"
       then Maze.Ascii
       else if str == "Colored"
               then Maze.Colored
               else Maze.Polar

---- INPUTS ----

-- wire the entire application together
main : Signal Html
main =
    Signal.map (view actions.address) model

userInput : Signal Action
userInput =
    Signal.mergeMany
        [ Signal.map LoadAsciiMask outputFromFileAscii
        , Signal.map LoadImageMask outputFromFilePNG
        , actions.signal
        ]

-- manage the model of our application over time
--model : Signal (Model a)
model =
    Signal.foldp update initialModel userInput

--initialModel : Model a
initialModel =
    {
        maze = Maze.init Maze.defaultAlgorithm initWidth initHeight startTimeSeed initDisplay
    }

-- actions from user input
actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp

startTimeSeed : Seed
-- uncomment to debug with consistent seed
--startTimeSeed = Random.initialSeed 123
startTimeSeed = Random.initialSeed <| round startTime

-- port to get current time for seeds
port startTime : Float

-- ports for file uploads
port outputFromFileAscii : Signal (List String)
port outputFromFileAscii = 
      Signal.map String.lines openFromTextFile

port outputFromFilePNG : Signal PngData
port outputFromFilePNG =
      openFromPNGFile

port openFromTextFile : Signal String
port openFromPNGFile : Signal PngData

