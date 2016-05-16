module Maze where

import Grid exposing (..)
import DistanceGrid
import ColoredGrid
--import WeightedGrid
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

import Random.PCG as Random exposing (Seed, initialSeed, split)
import Html exposing (pre, br, text, div)
import Html.Attributes exposing (..)
import Graphics.Element exposing (Element)

type Algorithm = NoOp
               | BinaryTree
               | Sidewinder
               --| AldousBroder
               --| Wilsons
               --| HuntAndKill
               --| RecursiveBacktracker

type alias AlgAttr = {
    alg : Algorithm,
    name : String
}

-- We could have fancier displays someday
type Display = Ascii
             | Colored

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
defaultBraidFactor = 0

-- Data formatted for html selects
displays = [(Ascii, "ASCII")
        , (Colored, "Colored")
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
           --Wilsons -> Wilsons.on randCellFn neighborFn
           --HuntAndKill -> HuntAndKill.on randCellFn neighborFn
           --RecursiveBacktracker -> RecursiveBacktracker.on randCellFn neighborFn

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

--update : Maze a -> Maze a
update maze =
    -- update rngs
    let -- apply maze generation algoritm
        grid' = maze.generator maze.grid <| Debug.log "step: " maze.step
        -- apply braiding
        grid'' = Grid.braid grid' (neighborsFn maze) maze.braidFactor
    in 
       { maze |
       grid = grid'',
       step = maze.step + 1
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
    in update maze'

--view : Maze a -> Html
view maze =
    let gridHtml = case maze.display of
            Ascii ->
                div [] [ pre [] [text <| GridRenderer.toAscii maze.grid Grid.cellToAscii]
                , viewDistances maze]

            Colored ->
                Html.fromElement <| mazeToElement maze

    in
       div [] [
           text <| (algToString maze.alg) ++ " algorithm"
           , br [] []
           , text <| (toString maze.grid.cols) ++ " X " ++ (toString maze.grid.rows)
           , br [] []
           , text <| (toString <| List.length (Grid.deadEnds maze.grid)) ++ " deadends"
           , gridHtml
           ]

--viewDistances : Maze a -> Html
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

-- Renders maze as an HTML element
mazeToElement : Maze a -> Element
mazeToElement maze =
    let cellPainter = ColoredGrid.cellBackgroundColor
        renderer = GridRenderer.toElement maze.grid
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
       renderer' ColoredGrid.cellBackgroundColor cellSize

-- returns available maze algorithms for the maze shape
algorithms : Shape -> List AlgAttr
algorithms shape =
    let algs = [ {alg = NoOp, name = algToString NoOp}]
        rectAlgs = [
            {alg = BinaryTree, name = algToString BinaryTree}
            , {alg = Sidewinder, name = algToString Sidewinder}
            --, {alg = HuntAndKill, name = algToString HuntAndKill}
        ]
        triangleAlgs = [
            --{alg = HuntAndKill, name = algToString HuntAndKill}
        ]
        allAlgs = [
            --{alg = AldousBroder, name = algToString AldousBroder}
            --, {alg = Wilsons, name = algToString Wilsons}
            --, {alg = RecursiveBacktracker, name = algToString RecursiveBacktracker}
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
        --AldousBroder -> "Aldous-Broder"
        --Wilsons -> "Wilsons"
        --HuntAndKill -> "Hunt - Kill"
        --RecursiveBacktracker -> "Recursive Backtracker"

algByName : String -> Algorithm
algByName str =
    let res = List.head <| List.filter (\a -> a.name == str) (algorithms Rect)
    in
       case res of
           Just a -> a.alg
           _ -> Debug.crash "Unknown algorithm" BinaryTree

