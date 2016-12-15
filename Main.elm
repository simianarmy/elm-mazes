module Main exposing (main)

import Maze exposing (..)
import Mask exposing (Mask)

import Debug
import Array exposing (Array)
import String
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Random exposing (Seed, initialSeed)
import Time exposing (Time, every)
import Slider

-- defaults
initWidth   = 5
initHeight  = 5
initDisplay = Maze.Colored
initShape   = Maze.Rect
-- controls speed of the generation (lower = faster)
mazeGenStepTime = 5

--- MODEL ---

-- The full application state
type alias AppState = 
    { maze : Maze
    , seedInitialized : Bool
    , seed: Random.Seed
    --, braidSlider: Slider.Model
    , totalTime: Float
    , genState: Generation
    }

type alias PngData =
    { width : Int,
      height : Int,
      blackFlags : Array Bool
    }

type alias Model = AppState
type Generation =
    Stepwise
    | Automatic

--- UPDATE ---

-- A description of the kinds of actions that can be performed on the model of
-- our application. See the following for more info on this pattern and
-- some alternatives: https://github.com/evancz/elm-architecture-tutorial/
type Msg = 
    NoOp
    | Tick Float
    -- | Prev
    | Next
    | Run
    | Stop
    | Refresh
    | UpdateWidth String
    | UpdateHeight String
    | SelectAlg String
    | SelectView Maze.Display
    | SelectShape Maze.Shape
    | Braid Slider.Msg
    | LoadAsciiMask (List String)
    | LoadImageMask PngData

-- How we update our Model on a given Msg?
update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp -> model

        Refresh ->
            {model | maze = Maze.reset model.maze}

        UpdateWidth str ->
            let maze_ = Maze.updateSize model.maze (String.toInt str |> Result.toMaybe |> Maybe.withDefault model.maze.grid.cols) model.maze.grid.rows
            in
               {model | maze = maze_}

        UpdateHeight str ->
            let maze_ = Maze.updateSize model.maze model.maze.grid.cols (String.toInt str |> Result.toMaybe |> Maybe.withDefault model.maze.grid.rows)
            in
               {model | maze = maze_}

        SelectAlg str ->
            let maze = model.maze
                maze_ = Maze.init (Maze.algByName str) maze.grid.cols maze.grid.rows maze.grid.rnd.seed maze.shape maze.display
            in
               {model | maze = maze_}

        SelectView display ->
            let maze = Maze.updateView model.maze <| Debug.log "display" display
            in
               {model | maze = maze}

        SelectShape shape ->
            -- new grid, re-init time
            let maze = model.maze
                maze_ = Maze.init maze.alg maze.grid.cols maze.grid.rows maze.grid.rnd.seed shape maze.display
            in
               {model | maze = maze_}

        Braid act ->
            -- TODO: elm-reactor 0.16 breaks on Slider.init!  Re-enable when it's fixed
            -- let factor = Result.withDefault Maze.defaultBraidFactor (String.toFloat model.braidSlider.value)
            --     maze_ = Maze.updateBraiding model.maze factor
            -- in
            --    {model |
            --        maze = maze_,
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
                    maze = Maze.update model.maze 1,
                    totalTime = 0
                }
                else
                case model.genState of
                    Stepwise -> model
                    Automatic -> {model |
                        totalTime = model.totalTime + dt
                    }

        -- Prev ->
        --     {model |
        --         maze = Maze.update model.maze -1,
        --         totalTime = 0
        --     }

        Next ->
            {model |
                maze = Maze.update model.maze 1,
                totalTime = 0
            }

        Run ->
            {model |
                genState = Automatic
            }

        Stop ->
            {model |
                genState = Stepwise
            }

--- VIEW ---
--view : Model -> Html
view : Model -> Html Msg
view model =
    let
        selectAlg = Html.Events.on "change" (Json.map SelectAlg targetValue)
        selectView = Html.Events.on "change" (Json.map SelectView targetValue)
        selectShape = Html.Events.on "change" (Json.map SelectShape targetValue)
        algToOptions attr =
            option [selected (attr.alg == model.maze.alg)] [text attr.name]
        viewToOption opt =
            option [selected ((Tuple.first opt) == model.maze.display)] [text (Tuple.second opt)]
        shapeToOption opt =
            option [selected ((Tuple.first opt) == model.maze.shape)] [text (Tuple.second opt)]
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
              , on "input" (Json.map UpdateWidth targetValue) ] []
        , text " X "
        , input [ class "sizeInput", value (toString maze.grid.rows)
              , on "input" (Json.map UpdateHeight targetValue)] []
        , br [] []
        , select [ selectAlg ] (List.map algToOptions <| Maze.algorithms maze.shape)
        , select [ selectView ] (List.map viewToOption Maze.displays)
        , select [ selectShape ] (List.map shapeToOption Maze.shapes)
        , br [] []
        , text "Braids (0 = max deadends, 1 = no deadends):"
        --, map Braid (Slider.view model.braidSlider)
        , br [] []
        -- , button [ onClick address Prev ] [ text "<" ]
        , button [ onClick Next ] [ text ">" ]
        , button [ onClick Run ] [ text "Run" ]
        , button [ onClick Stop ] [ text "Stop" ]
        , button [ onClick Refresh ] [ text "REFRESH" ]
        , br [] []
        , text "Ascii Mask file: "
        , input [ type_ "file", id "maskfileinput" ] []
        , footer [] []
        ]

-- this seems like not the right way to do it
displayFromString : String -> Display
displayFromString str =
    if str == "ASCII"
       then Maze.Ascii
       else if str == "Colored"
               then Maze.Colored
               else if str == "Weighted"
                       then Maze.Weighted
                       else Maze.Colored

shapeFromString : String -> Shape
shapeFromString str =
    let s = List.filter (\e -> (Tuple.second e) == str) Maze.shapes
    in
       case List.head s of
           Nothing -> initShape
           Just item -> Tuple.first item

---- INPUTS ----

-- wire the entire application together
main : Program Never
main = Html.program {
        model = initialModel,
        view = view,
        update = update,
        subscriptions = \_ -> Sub.none
    }

--userInput : Signal Msg
--userInput =
    --Signal.mergeMany
        --[ 
        --actions.signal
        --, tick
        ---- Signal.map LoadAsciiMask outputFromFileAscii
        ----, Signal.map LoadImageMask outputFromFilePNG
        --]

-- manage the model of our application over time
--model : Signal (Model)
--model =
    --Signal.foldp update initialModel userInput

initialModel : Model
initialModel =
    {
        maze = Maze.init Maze.defaultAlgorithm initWidth initHeight startTimeSeed initShape initDisplay
      , seedInitialized = False
      , seed = initialSeed 45 -- This will not get used.
      --, braidSlider = Slider.init { id="braid", label="Braid Factor", value=(toString Maze.defaultBraidFactor), min=0, max=1, step=0.1 }
      , totalTime = 0.0
      , genState = Stepwise
    }


--tick : Signal Msg 
--tick = Signal.map (\dt -> Tick dt) (fps 16)

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
