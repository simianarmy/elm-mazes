module Main where

import Maze exposing (..)
import Mask exposing (Mask)

import Debug
import Array exposing (Array)
import String
import Signal exposing (Signal, Address)
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import Random.PCG as Random exposing (Seed, initialSeed, split)
import Time exposing (Time, every, fps)
import Slider

initWidth   = 5
initHeight  = 5
initDisplay = Maze.Colored
initShape   = Maze.Rect
-- controls speed of the generation (lower = faster)
mazeGenStepTime = 50

--- MODEL ---

-- The full application state
type alias AppState a = 
    { maze : Maze a
    , seedInitialized : Bool
    , seed: Random.Seed
    --, braidSlider: Slider.Model
    , totalTime: Float
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
    | Tick Float
    | Refresh
    | UpdateWidth String
    | UpdateHeight String
    | SelectAlg String
    | SelectView Maze.Display
    | SelectShape Maze.Shape
    | Braid Slider.Action
    | LoadAsciiMask (List String)
    | LoadImageMask PngData

-- How we update our Model on a given Action?
--update : Action -> Model a -> Model a
update action model =
    case action of
        NoOp -> model

        Refresh ->
            {model | maze = Maze.reset model.maze}

        UpdateWidth str ->
            let maze' = Maze.updateSize model.maze (String.toInt str |> Result.toMaybe |> Maybe.withDefault model.maze.grid.cols) model.maze.grid.rows
            in
               {model | maze = maze'}

        UpdateHeight str ->
            let maze' = Maze.updateSize model.maze model.maze.grid.cols (String.toInt str |> Result.toMaybe |> Maybe.withDefault model.maze.grid.rows)
            in
               {model | maze = maze'}

        SelectAlg str ->
            let maze = model.maze
                maze' = Maze.init (Maze.algByName str) maze.grid.cols maze.grid.rows maze.grid.rnd.seed maze.shape maze.display
            in
               {model | maze = Maze.update maze'}

        SelectView display ->
            let maze' = Maze.updateView model.maze display
            in
               {model | maze = maze'}

        SelectShape shape ->
            -- new grid, re-init time
            let maze = model.maze
                maze' = Maze.init maze.alg maze.grid.cols maze.grid.rows maze.grid.rnd.seed shape maze.display
            in
               {model | maze = maze'}

        Braid act ->
            -- TODO: elm-reactor 0.16 breaks on Slider.init!  Re-enable when it's fixed
            -- let factor = Result.withDefault Maze.defaultBraidFactor (String.toFloat model.braidSlider.value)
            --     maze' = Maze.updateBraiding model.maze factor
            -- in
            --    {model |
            --        maze = maze',
            --        braidSlider = Slider.update act model.braidSlider
            --    }
            model

        LoadAsciiMask lines ->
            let mask = Mask.fromTxt lines
            in
               {model | maze = Maze.setMask model.maze mask}

        LoadImageMask png ->
            let mask = Mask.fromImage (png.width, png.height) png.blackFlags
            in
               {model | maze = Maze.setMask model.maze mask}

        Tick dt ->
            -- We can use this to display the maze-generation incrementally
            if ((truncate model.totalTime) >= mazeGenStepTime)
                then {model |
                    maze = Maze.update model.maze,
                    totalTime = 0
                }
                else {model |
                    totalTime = model.totalTime + dt
                }

--- VIEW ---
--view : Address Action -> Model a -> Html
view address model =
    let
        selectAlg = Html.Events.on "change" targetValue
            (\val -> Signal.message address <| SelectAlg <| val)
        selectView = Html.Events.on "change" targetValue
            (\val -> Signal.message address <| SelectView <| displayFromString val)
        selectShape = Html.Events.on "change" targetValue
            (\val -> Signal.message address <| SelectShape <| shapeFromString val)
        algToOptions attr =
            option [selected (attr.alg == model.maze.alg)] [text attr.name]
        viewToOption opt =
            option [selected ((fst opt) == model.maze.display)] [text (snd opt)]
        shapeToOption opt =
            option [selected ((fst opt) == model.maze.shape)] [text (snd opt)]
        maze = model.maze
    in
    div [ id "main" ] [
        -- title
        header [] [ h1 [] [text "Amaze Mazes" ]]
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
        , select [ selectAlg ] (List.map algToOptions <| Maze.algorithms maze.shape)
        , select [ selectView ] (List.map viewToOption Maze.displays)
        , select [ selectShape ] (List.map shapeToOption Maze.shapes)
        , br [] []
        , text "Braids (1 = no deadends):"
        --, Slider.view (Signal.forwardTo address Braid) model.braidSlider
        , button [ onClick address Refresh ] [ text "REFRESH" ]
        , br [] []
        , text "Ascii Mask file: "
        , input [ type' "file", id "maskfileinput" ] []
        , footer [] []
        ]

displayFromString : String -> Display
displayFromString str =
    if str == "ASCII"
       then Maze.Ascii
       else Maze.Colored

shapeFromString str =
    let s = List.filter (\e -> (snd e) == str) Maze.shapes
    in
       case List.head s of
           Nothing -> initShape
           Just item -> fst item

---- INPUTS ----

-- wire the entire application together
main : Signal Html
main =
    Signal.map (view actions.address) model

userInput : Signal Action
userInput =
    Signal.mergeMany
        [ 
        actions.signal
        , tick
        -- Signal.map LoadAsciiMask outputFromFileAscii
        --, Signal.map LoadImageMask outputFromFilePNG
        ]

-- manage the model of our application over time
--model : Signal (Model a)
model =
    Signal.foldp update initialModel userInput

--initialModel : Model a
initialModel =
    {
        maze = Maze.init Maze.defaultAlgorithm initWidth initHeight startTimeSeed initShape initDisplay
      , seedInitialized = False
      , seed = initialSeed 45 -- This will not get used.
      --, braidSlider = Slider.init { id="braid", label="Braid Factor", value=(toString Maze.defaultBraidFactor), min=0, max=1, step=0.1 }
      , totalTime = 0.0
    }

-- actions from user input
actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp

tick : Signal Action 
tick  = Signal.map (\dt -> Tick dt) (fps 16)

startTimeSeed : Random.Seed
-- uncomment to debug with consistent seed
startTimeSeed = Random.initialSeed 123
--startTimeSeed = Random.initialSeed <| round startTime

-- port to get current time for seeds
--port startTime : Float

-- ports for file uploads
--port outputFromFileAscii : Signal (List String)
--port outputFromFileAscii = 
--      Signal.map String.lines openFromTextFile
--
--port outputFromFilePNG : Signal PngData
--port outputFromFilePNG =
--      openFromPNGFile
--
--port openFromTextFile : Signal String
--port openFromPNGFile : Signal PngData
--
