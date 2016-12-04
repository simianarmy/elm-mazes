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
import Mask
import GridCell exposing (GridCell)
import BinaryTree
import Sidewinder
import AldousBroder
import Wilsons
import HuntAndKill
import RecursiveBacktracker 
import GridUtils

import Random exposing (Seed, initialSeed, split) 
import Html exposing (pre, br, text, div)
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

type alias Maze a = {
    grid : Grid a,
    alg : Algorithm,
    shape: Shape,
    display : Display,
    generator : Grid a -> Int -> Grid a,
    braidFactor : Float,
    step: Int
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


--init : Algorithm -> Int -> Int -> Seed -> Shape -> Display -> Maze a
init algType width height seed shape display =
    let mask = Mask.createMask width height
        cellGenFn = case shape of
            Rect -> Grid.makeCells
            Polar -> PolarGrid.makeCells
            Hex -> HexGrid.makeCells
            Triangle -> TriangleGrid.makeCells
        grid' = Grid.createGridFromMask mask seed cellGenFn
    in
       {
           grid = grid',
           generator = genAlg algType shape,
           alg = algType,
           shape = shape,
           display = display,
           braidFactor = defaultBraidFactor,
           step = 0
       }

-- generates maze algorithm function taking a grid and returning a grid
genAlg : Algorithm -> Shape -> (Grid a -> Int -> Grid a)
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
neighborsFn : Maze a -> (Grid a -> GridCell -> List GridCell)
neighborsFn maze =
    case maze.shape of
        Rect -> Grid.neighbors
        Polar -> PolarGrid.neighbors
        Hex -> HexGrid.neighbors
        Triangle -> TriangleGrid.neighbors

reset : Maze a -> Maze a
reset maze =
    {maze |
        grid = Grid.reset maze.grid,
        step = 0
    }

braid : Maze a -> Maze a
braid maze =
    let bgrid = Grid.braid maze.grid (neighborsFn maze) maze.braidFactor
    in
       { maze |
       grid = bgrid
       }

update : Maze a -> Int -> Maze a
update maze step =
    -- update rngs
    let -- apply maze generation algoritm
        grid' = maze.generator maze.grid maze.step
    in 
       { maze |
       grid = grid',
       step = Debug.watch "maze step" <| maze.step + step 
       }

-- INVALIDATES MASK, SO REFRESH MAZE
--updateSize : Maze a -> Int -> Int -> Maze a
updateSize maze width height =
    init maze.alg width height maze.grid.rnd.seed maze.shape maze.display

-- updateView : Maze a -> Display -> Maze b
updateView maze displayType =
    {maze | display = displayType}

-- setMask : Maze a -> Mask -> Maze a
setMask maze mask =
    let grid' = Grid.createGridFromMask mask maze.grid.rnd.seed maze.grid.cellMaker
        -- we want to run the algorithm to completion now
    in {maze | grid = maze.generator grid' (Grid.size maze.grid)}

updateBraiding: Maze a -> Float -> Maze a
updateBraiding maze factor =
    let maze' = {maze | braidFactor = factor}
    in update maze' 1

--view : Maze a -> Html
view maze =
    let braided = braid maze
        gridHtml = case maze.display of
            Ascii ->
                div [] [
                       text <| (toString <| List.length (Grid.deadEnds maze.grid)) ++ " deadends"
                       , pre [] [text <| GridRenderer.toAscii maze.grid Grid.cellToAscii]
                       --, text <| "With braid factor " ++ (toString braided.braidFactor)
                       --, br [] []
                       --, text <| (toString <| List.length (Grid.deadEnds braided.grid)) ++ " deadends"
                       --, pre [] [text <| GridRenderer.toAscii braided.grid Grid.cellToAscii]
                        , viewDistances maze]

            Colored ->
                Html.fromElement <| mazeToElement maze

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

--viewDistances : Maze a -> Html.Html
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
viewWeightedDistances : Maze a -> Html.Html
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
          , Html.fromElement <| GridRenderer.toWeightedElement wgrid Grid.painter cellSize
          , text <| "Shortest path from " ++ (GridCell.toString start) ++ " to :" ++ (GridCell.toString finish)
          , Html.fromElement <| GridRenderer.toWeightedElement shortestPathGrid Grid.painter cellSize
          , if False --(Maybe.withDefault -1 <| Dict.get (GridCell.id finish) pathDistances.cells) == -1
               then text "N/A"
               else viewWeightedDistancesWithLava wgrid.dgrid.grid start finish
          --, pre [] [text <| DistanceGrid.viewDistances shortestPathGrid.dgrid]
          -- , pre [] [text <| DistanceGrid.viewDistances shortestPathGrid]
          -- , text "Longest path in maze:"
          -- , pre [] [text <| DistanceGrid.viewDistances longestPathGrid]
           ]

viewWeightedDistancesWithLava : Grid a -> GridCell -> GridCell -> Html.Html
viewWeightedDistancesWithLava grid start goal =
    let -- pick cell on path to be lava
        pathCells = List.filter (\gc -> (GridCell.base gc).weight > 0) <| Grid.cellsList grid.cells
        -- As much as I'd like to pick a random cell from the shortest path, the randomness makes it shitty
        -- sample = GridCell.maybeGridCellToGridCell <| GridUtils.sampleCell pathCells grid.rnd
        -- sample = GridCell.maybeGridCellToGridCell <| List.head <| List.reverse <| List.take ((List.length pathCells) // 2) pathCells
        -- Best to pick a cell near the beginning or end I guess...
        sample = GridCell.maybeGridCellToGridCell <| Grid.getCell grid 0 1
        lava = GridCell.setWeight sample 50
        lavaGrid = Grid.updateCellById grid (GridCell.id lava) lava
        lavaDGrid = WeightedGrid.createGrid lavaGrid start
        lavaPathDistances = WeightedGrid.pathTo lavaDGrid goal
        lavaPathGrid = {lavaDGrid | dists = lavaPathDistances}
    in
       div [] [
          text <| "Shortest path with lava from " ++ (GridCell.toString start) ++ " to :" ++ (GridCell.toString goal)
          , Html.fromElement <| GridRenderer.toWeightedElement lavaPathGrid Grid.painter cellSize
          ]

-- Renders maze as an HTML element
mazeToElement : Maze a -> Element
mazeToElement maze =
    let renderer = GridRenderer.toColoredElement maze.grid
        renderer' = case maze.shape of
            Rect ->
                let root = Grid.center maze.grid
                in
                   renderer Grid.painter root

            Polar ->
                let root = PolarGrid.center maze.grid
                in
                   renderer PolarGrid.painter root

            Hex ->
                let root = Grid.center maze.grid
                in
                   renderer HexGrid.painter root

            Triangle ->
                let root = Grid.center maze.grid
                in
                   renderer TriangleGrid.painter root
    in
       renderer' cellSize

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

