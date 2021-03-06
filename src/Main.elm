port module Main exposing (main)

import Maze exposing (..)
import Mask exposing (Mask)

import Debug
import Array exposing (Array)
import String
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (..)
import Random exposing (Seed, initialSeed)
import Time exposing (Time, every)
import AnimationFrame exposing (..)
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
    , braidSlider: Slider.Model
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
    | NewTimeSeed Int
    | Tick Float
    -- | Prev
    | Next
    | Run
    | Stop
    | Refresh
    | UpdateWidth String
    | UpdateHeight String
    | SelectAlg String
    | SelectView String
    | SelectShape String
    | Braid Slider.Msg
    | LoadAsciiMask (List String)
    | LoadImageMask PngData

-- How we update our Model on a given Msg?
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)

        NewTimeSeed seed ->
            ({model | seed = Random.initialSeed (Debug.log "seed from port: " seed), seedInitialized = True}
            , Cmd.none
            )

        Refresh ->
            ({model | maze = Maze.reset model.maze}
            , Cmd.none
            )

        UpdateWidth str ->
            let maze_ = Maze.updateSize model.maze (String.toInt str |> Result.toMaybe |> Maybe.withDefault model.maze.grid.cols) model.maze.grid.rows
            in
               ({model | maze = maze_}
               , Cmd.none
               )

        UpdateHeight str ->
            let maze_ = Maze.updateSize model.maze model.maze.grid.cols (String.toInt str |> Result.toMaybe |> Maybe.withDefault model.maze.grid.rows)
            in
               ({model | maze = maze_}
               , Cmd.none
               )

        SelectAlg str ->
            let maze = model.maze
                maze_ = Maze.init (Maze.algByName str) maze.grid.cols maze.grid.rows maze.grid.rnd.seed maze.shape maze.display
            in
               ({model | maze = maze_}
               , Cmd.none
               )

        SelectView display ->
            let maze = Maze.updateView model.maze <| Debug.log "display" (displayFromString display)
            in
               ({model | maze = maze}
               , Cmd.none
               )

        SelectShape shape ->
            -- new grid, re-init time
            let maze = model.maze
                maze_ = Maze.init maze.alg maze.grid.cols maze.grid.rows maze.grid.rnd.seed (shapeFromString shape) maze.display
            in
               ({model | maze = maze_}
               , Cmd.none
               )

        Braid act ->
            let factor = Result.withDefault Maze.defaultBraidFactor (String.toFloat model.braidSlider.value)
                maze_ = Maze.updateBraiding model.maze factor
            in
               ({model |
                   maze = maze_,
                   braidSlider = Slider.update act model.braidSlider
               }
               , Cmd.none
               )

        LoadAsciiMask lines ->
            let mask = Mask.fromTxt lines
            in
               ({model | maze = Maze.setMask model.maze mask}
               , Cmd.none
               )

        LoadImageMask png ->
            let mask = Mask.fromImage (png.width, png.height) png.blackFlags
            in
               ({model | maze = Maze.setMask model.maze mask}
               , Cmd.none
               )

        Tick dt ->
            -- We can use this to display the maze-generation incrementally
            let model_ = if ((truncate model.totalTime) >= mazeGenStepTime)
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
            in
               (model_, Cmd.none)

        -- Prev ->
        --     {model |
        --         maze = Maze.update model.maze -1,
        --         totalTime = 0
        --     }

        Next ->
            ({model |
                maze = Maze.update model.maze 1,
                totalTime = 0
            }
            , Cmd.none
            )

        Run ->
            ({model |
                genState = Automatic
            }
            , Cmd.none
            )

        Stop ->
            ({model |
                genState = Stepwise
            }
            , Cmd.none
            )

--- VIEW ---
--view : Model -> Html
view : Model -> Html Msg
view model =
    let
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
        , input [ class "sizeInput", HA.value (toString maze.grid.cols)
              , on "input" (Json.map UpdateWidth targetValue) ] []
        , text " X "
        , input [ class "sizeInput", HA.value (toString maze.grid.rows)
              , on "input" (Json.map UpdateHeight targetValue)] []
        , br [] []
        , select [ on "change" (Json.map SelectAlg targetSelectedOption) ] (List.map algToOptions <| Maze.algorithms maze.shape)
        , select [ on "change" (Json.map SelectView targetSelectedOption) ] (List.map viewToOption Maze.displays)
        , select [ on "change" (Json.map SelectShape targetSelectedOption) ] (List.map shapeToOption Maze.shapes)
        , br [] []
        , text "Braids (0 = max deadends, 1 = no deadends):"
        , Html.map Braid (Slider.view model.braidSlider)
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

-- Form select helpers

targetSelectedOption : Json.Decoder String
targetSelectedOption =
    at ["target", "value"] string

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
--main : Program Never
main = Html.program {
        init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

initialModel : Model
initialModel =
    {
        maze = Maze.init Maze.defaultAlgorithm initWidth initHeight startTimeSeed initShape initDisplay
      , seedInitialized = False
      , seed = initialSeed 45 -- This will not get used.
      , braidSlider = Slider.init { id="braid", label="Braid Factor", value=(toString Maze.defaultBraidFactor), min=0, max=1, step=0.1 }
      , totalTime = 0.0
      , genState = Stepwise
    }

init : (Model, Cmd Msg)
init =
    (initialModel, Cmd.none)

startTimeSeed : Random.Seed
-- uncomment to debug with consistent seed
startTimeSeed = Random.initialSeed 123
--startTimeSeed = Random.initialSeed <| round startTime

-- port to get current time for seeds
port startTime : (Int -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ AnimationFrame.diffs Tick
    , startTime NewTimeSeed
    ]

--userInput : Signal Msg
--userInput =
    --Signal.mergeMany
        --[ 
        --actions.signal
        --, tick
        ---- Signal.map LoadAsciiMask outputFromFileAscii
        ----, Signal.map LoadImageMask outputFromFilePNG
        --]

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
