module Maze exposing (..)

import Grid exposing (..)
import DistanceGrid
import ColoredGrid
import WeightedGrid
import PolarGrid
import HexGrid
import TriangleGrid
import GridRenderer
import Rnd
import Mask exposing (Mask)
import GridCell exposing (GridCell)
import BinaryTree
import Sidewinder
import AldousBroder
import Wilsons
import HuntAndKill
import RecursiveBacktracker 
import GridUtils
import Distances

import Random exposing (Seed, initialSeed) 
import Html exposing (..)
import Html.Attributes exposing (..)
import Element exposing (Element)
import Dict

type Algorithm = NoOp
               | BinaryTree
               | Sidewinder
               | AldousBroder
               | Wilsons
               | HuntAndKill
               | RecursiveBacktracker

type alias AlgAttr = {
    alg : Algorithm,
    name : String
}

-- We could have fancier displays someday
type Display = Ascii
             | Colored
             | Weighted

type Shape = Rect
            | Polar
            | Hex
            | Triangle

type alias MazeGenerator = Grid -> Int -> Grid

type alias Maze = {
    grid : Grid,
    alg : Algorithm,
    shape: Shape,
    display : Display,
    generator : MazeGenerator,
    braidFactor : Float,
    step : Int,
    complete : Bool
}

defaultAlgorithm = NoOp
defaultBraidFactor = 0.5

-- Data formatted for html selects
displays = [(Ascii, "ASCII")
        , (Colored, "Colored")
        , (Weighted, "Weighted")
        ]
shapes = [(Rect, "Rectangular")
        , (Polar, "Polar")
        , (Hex, "Hexagonal")
        , (Triangle, "Triangle")
        ]

cellSize : Int
cellSize = 30

cellInset : Float
cellInset = 0.1


init : Algorithm -> Int -> Int -> Seed -> Shape -> Display -> Maze
init algType width height seed shape display =
    let mask = Mask.createMask width height
        cellGenFn = case shape of
            Rect -> Grid.makeCells
            Polar -> PolarGrid.makeCells
            Hex -> HexGrid.makeCells
            Triangle -> TriangleGrid.makeCells
        grid_ = Grid.createGridFromMask mask seed cellGenFn
    in
       {
           grid = grid_,
           generator = genAlg algType shape,
           alg = algType,
           shape = shape,
           display = display,
           braidFactor = defaultBraidFactor,
           step = 0,
           complete = False
       }

-- generates maze algorithm function taking a grid and returning a grid
genAlg : Algorithm -> Shape -> MazeGenerator
genAlg algName shape =
    let randCellFn = case shape of
            Polar -> PolarGrid.randomCell
            _ -> Grid.randomCell
        neighborFn = case shape of
            Polar -> PolarGrid.neighbors
            Hex -> HexGrid.neighbors
            Triangle -> TriangleGrid.neighbors
            _ -> Grid.neighbors
    in
       --BinaryTree.step randCellFn neighborFn
       case algName of
           NoOp -> always
           --BinaryTree -> BinaryTree.on randCellFn neighborFn
           BinaryTree -> BinaryTree.step randCellFn neighborFn
           --Sidewinder -> Sidewinder.on randCellFn neighborFn
           Sidewinder -> Sidewinder.step randCellFn neighborFn
           --AldousBroder -> AldousBroder.on randCellFn neighborFn
           AldousBroder -> AldousBroder.step randCellFn neighborFn
           --Wilsons -> Wilsons.on randCellFn neighborFn
           Wilsons -> Wilsons.step randCellFn neighborFn
           --HuntAndKill -> HuntAndKill.on randCellFn neighborFn
           HuntAndKill -> HuntAndKill.step randCellFn neighborFn
           -- RecursiveBacktracker -> RecursiveBacktracker.on randCellFn neighborFn
           RecursiveBacktracker -> RecursiveBacktracker.step randCellFn neighborFn

-- returns neighbors function for the grid type
neighborsFn : Maze -> (Grid -> GridCell -> List GridCell)
neighborsFn maze =
    case maze.shape of
        Rect -> Grid.neighbors
        Polar -> PolarGrid.neighbors
        Hex -> HexGrid.neighbors
        Triangle -> TriangleGrid.neighbors

reset : Maze -> Maze
reset maze =
    {maze |
        grid = Grid.reset maze.grid,
        step = 0,
        complete = False
    }

braid : Maze -> Maze
braid maze =
    let bgrid = Grid.braid maze.grid (neighborsFn maze) maze.braidFactor
    in
       { maze |
       grid = bgrid
       }

update : Maze -> Int -> Maze
update maze step =
    -- update rngs
    let -- apply maze generation algoritm
        grid_ = maze.generator maze.grid maze.step
    in
       { maze |
       grid = grid_,
       step = Debug.log "maze step" <| maze.step + step 
       }

-- INVALIDATES MASK, SO REFRESH MAZE
updateSize : Maze -> Int -> Int -> Maze
updateSize maze width height =
    init maze.alg width height maze.grid.rnd.seed maze.shape maze.display

updateView : Maze -> Display -> Maze
updateView maze displayType =
    {maze | display = displayType}

setMask : Maze -> Mask -> Maze
setMask maze mask =
    let grid_ = Grid.createGridFromMask mask maze.grid.rnd.seed maze.grid.cellMaker
        -- we want to run the algorithm to completion now
    in {maze | grid = maze.generator grid_ (Grid.size maze.grid)}

updateBraiding: Maze -> Float -> Maze
updateBraiding maze factor =
    let maze_ = {maze | braidFactor = factor}
    in update maze_ 1

view : Maze -> Html msg
view maze =
    let braided = braid maze
        displayMetadata m =
            div [] [
                text <| "Braid factor " ++ (toString m.braidFactor)
                , br [] []
                , text <| (toString <| List.length (Grid.deadEnds m.grid)) ++ " deadends"
                ]
        gridHtml = case maze.display of
            Ascii ->
                div [] [
                       text <| (toString <| List.length (Grid.deadEnds maze.grid)) ++ " deadends"
                       , pre [] [text <| GridRenderer.toAscii maze.grid Grid.cellToAscii]
                       , displayMetadata braided
                       , pre [] [text <| GridRenderer.toAscii braided.grid Grid.cellToAscii]
                        , viewDistances maze]

            Colored ->
                div [] [
                    displayMetadata maze
                   , Element.toHtml <| mazeToElement maze
                   ]

            Weighted ->
                viewWeightedDistances <| braid {maze | braidFactor = 0.5}

    in
       div [] [
           text <| (algToString maze.alg) ++ " algorithm"
           , br [] []
           , text <| (toString maze.grid.cols) ++ " X " ++ (toString maze.grid.rows)
           , br [] []
           , gridHtml
           ]

viewDistances : Maze -> Html msg
viewDistances maze =
    let center = Grid.center maze.grid
        start = center
        goal = GridCell.maybeGridCellToGridCell <| getCell maze.grid 0 0
        dgrid = DistanceGrid.createGrid maze.grid center
        pathDistances = DistanceGrid.pathTo dgrid start goal
        shortestPathGrid = {dgrid | dists = pathDistances}
        longDistances = DistanceGrid.longestPath dgrid center
        longestPathGrid = {dgrid | dists = longDistances}
        rootStr = GridCell.toString center
    in
       div [] [
          br [] [] 
          , text <| "Cell distances from " ++ rootStr ++ ":"
          , pre [] [text <| DistanceGrid.viewDistances dgrid]
          , text <| "Shortest path from " ++ (GridCell.toString start) ++ " to :" ++ (GridCell.toString goal)
          , pre [] [text <| DistanceGrid.viewDistances shortestPathGrid]
          , text "Longest path in maze:"
          , pre [] [text <| DistanceGrid.viewDistances longestPathGrid]
           ]

-- displays braided maze with and without lava in the path
viewWeightedDistances : Maze -> Html msg
viewWeightedDistances maze =
    let start = GridCell.maybeGridCellToGridCell <| Grid.getCell maze.grid 0 0
        finish = GridCell.maybeGridCellToGridCell <| getCell maze.grid (maze.grid.rows - 1) (maze.grid.cols - 1)
        wgrid = WeightedGrid.createGrid maze.grid start
        pathDistances = WeightedGrid.pathTo wgrid finish
        shortestPathGrid = {wgrid | dists = pathDistances}
    in
       div [] [
          text <| "Cell distances from " ++ (GridCell.toString start)
          , br [] []
          , Element.toHtml <| GridRenderer.toWeightedElement wgrid Grid.painter cellSize cellInset
          , text <| "Shortest path from " ++ (GridCell.toString start) ++ " to :" ++ (GridCell.toString finish)
          ++ " Distance: " ++ (toString <| Dict.size pathDistances.cells)
          , Element.toHtml <| GridRenderer.toWeightedElement shortestPathGrid Grid.painter cellSize cellInset
          -- long way of checking if the maze isn't complete yet
          , if not <| isGenerated maze
               then text "N/A"
               else
               -- non-lava maze is complete, take a cell from it's shortest path dictionary
               let path = Distances.cells pathDistances
                   midCellID = List.head <| List.reverse <| List.take ((List.length path) // 2) path
                   midCell = case midCellID of
                       Just d -> Grid.cellIdToCell wgrid.dgrid.grid d
                       Nothing -> Debug.crash "nope"
               in
                   viewWeightedDistancesWithLava wgrid.dgrid.grid start finish midCell
           ]

viewWeightedDistancesWithLava : Grid
    -- start & finish cells
    -> GridCell -> GridCell
    -- lava cell
    -> GridCell
    -> Html msg
viewWeightedDistancesWithLava grid start goal lava =
    let lava_ = GridCell.setWeight lava 50
        lavaGrid = Grid.updateCellById grid (GridCell.id lava_) lava_
        lavaDGrid = WeightedGrid.createGrid lavaGrid start
        lavaPathDistances = WeightedGrid.pathTo lavaDGrid goal
        lavaPathGrid = {lavaDGrid | dists = lavaPathDistances}
    in
       div [] [
          text <| "Shortest path with lava from " ++ (GridCell.toString start) ++ " to :" ++ (GridCell.toString goal)
          ++ " Distance: " ++ (toString <| Dict.size lavaPathDistances.cells)
          , Element.toHtml <| GridRenderer.toWeightedElement lavaPathGrid Grid.painter cellSize cellInset
          ]

-- Renders maze as an HTML element
mazeToElement : Maze -> Element
mazeToElement maze =
    let renderer = GridRenderer.toColoredElement maze.grid
    in
        case maze.shape of
            Rect ->
                let root = Grid.center maze.grid
                in
                   renderer Grid.painter root cellSize cellInset

            Polar ->
                let root = PolarGrid.center maze.grid
                in
                   renderer PolarGrid.painter root cellSize 0

            Hex ->
                let root = Grid.center maze.grid
                in
                   renderer HexGrid.painter root cellSize 0

            Triangle ->
                let root = Grid.center maze.grid
                in
                   renderer TriangleGrid.painter root cellSize 0

-- returns available maze algorithms for the maze shape
algorithms : Shape -> List AlgAttr
algorithms shape =
    let algs = [ {alg = NoOp, name = algToString NoOp}]
        rectAlgs = [
            {alg = BinaryTree, name = algToString BinaryTree}
            , {alg = Sidewinder, name = algToString Sidewinder}
            , {alg = HuntAndKill, name = algToString HuntAndKill}
        ]
        triangleAlgs = [
            {alg = HuntAndKill, name = algToString HuntAndKill}
        ]
        allAlgs = [
            {alg = AldousBroder, name = algToString AldousBroder}
            , {alg = Wilsons, name = algToString Wilsons}
            , {alg = RecursiveBacktracker, name = algToString RecursiveBacktracker}
        ]
    in
       case shape of
           Polar -> List.concat [algs, allAlgs]
           Triangle -> List.concat [algs, triangleAlgs, allAlgs]
           _ -> List.concat [algs, rectAlgs, allAlgs]

-- returns true iff maze generation is complete
isGenerated : Maze -> Bool
isGenerated maze =
    let isUnlinkedCell gc =
        List.isEmpty <| Grid.linkedCells maze.grid gc
    in
       not <| List.any isUnlinkedCell <| Grid.cellsList maze.grid.cells

algToString : Algorithm -> String
algToString algType =
    case algType of
        NoOp -> "None"
        BinaryTree -> "Binary Tree"
        Sidewinder -> "Sidewinder"
        AldousBroder -> "Aldous-Broder"
        Wilsons -> "Wilsons"
        HuntAndKill -> "Hunt - Kill"
        RecursiveBacktracker -> "Recursive Backtracker"

algByName : String -> Algorithm
algByName str =
    let res = List.head <| List.filter (\a -> a.name == str) (algorithms Rect)
    in
       case res of
           Just a -> a.alg
           _ -> Debug.crash "Unknown algorithm" BinaryTree

