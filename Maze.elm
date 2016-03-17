module Maze where

import Grid exposing (..)
import DistanceGrid
import ColoredGrid
import PolarGrid
import HexGrid
import TriangleGrid
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
import Html exposing (..)
import Html.Attributes exposing (..)

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

type Shape = Rect
            | Polar
            | Hex
            | Triangle

type alias Maze a = {
    grid : Grid a,
    alg : Algorithm,
    shape: Shape,
    display : Display,
    generator : Grid a -> Grid a
}

defaultAlgorithm = NoOp

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
           display = display
       }

-- generates maze algorithm function taking a grid and returning a grid
genAlg : Algorithm -> Shape -> (Grid a -> Grid a)
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
       case algName of
           NoOp -> identity
           BinaryTree -> BinaryTree.on randCellFn neighborFn
           Sidewinder -> Sidewinder.on randCellFn neighborFn
           AldousBroder -> AldousBroder.on randCellFn neighborFn
           Wilsons -> Wilsons.on randCellFn neighborFn
           HuntAndKill -> HuntAndKill.on randCellFn neighborFn
           RecursiveBacktracker -> RecursiveBacktracker.on randCellFn neighborFn


--update : Maze a -> Maze a
update maze =
    let grid' = Grid.update maze.grid
    in {maze | grid = maze.generator grid'}

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
    in {maze | grid = maze.generator grid'}

--view : Maze a -> Html
view maze =
    let gridHtml = case maze.display of
            Ascii ->
                pre [] [text <| Grid.toAscii maze.grid Grid.cellToAscii]

            Colored ->
                case maze.shape of
                    Rect ->
                        let root = Grid.center maze.grid
                            coloredGrid = ColoredGrid.createGrid maze.grid root
                        in
                           fromElement <| Grid.toElement coloredGrid Grid.painter ColoredGrid.cellBackgroundColor cellSize

                    Polar ->
                        let (root, _) = GridCell.toPolarCell <| PolarGrid.center maze.grid
                            coloredGrid = ColoredGrid.createGrid maze.grid root
                        in
                            fromElement <| Grid.toElement coloredGrid PolarGrid.painter ColoredGrid.cellBackgroundColor cellSize

                    Hex ->
                        let root = Grid.center maze.grid
                            coloredGrid = ColoredGrid.createGrid maze.grid root
                        in
                           fromElement <| Grid.toElement coloredGrid HexGrid.painter ColoredGrid.cellBackgroundColor cellSize

                    Triangle ->
                        text "TRIANGLE MAZE HERE"

    in
       div [] [
           text <| (algToString maze.alg) ++ " algorithm"
           , br [] []
           , text <| (toString maze.grid.cols) ++ " X " ++ (toString maze.grid.rows)
           , br [] []
           , text <| (toString <| List.length (Grid.deadEnds maze.grid)) ++ " deadends"
           , gridHtml
           , br [] []
           --, viewDistances maze
           ]

--viewDistances : Maze a -> Html
viewDistances maze =
    let --root = toValidCell <| getCell maze.grid 1 1
        root = Grid.center maze.grid
        --goal = toValidCell <| getCell maze.grid maze.grid.rows 1
        dgrid = DistanceGrid.createGrid maze.grid root
        --pathDistances = DistanceGrid.pathTo maze.grid root goal
        --pathGrid = {dgrid | dists = pathDistances}
        --longDistances = DistanceGrid.longestPath maze.grid root
        --longGrid = {dgrid | dists = longDistances}
        --rootStr = Cell.cellToString root
    in
       div [] [
          br [] [] 
           --, text <| "Cell distances from " ++ rootStr ++ ":"
           , pre [] [text <| DistanceGrid.viewDistances dgrid]
           --, text <| "Shortest path from " ++ rootStr ++ " to SW corner:"
           --, pre [] [text <| DistanceGrid.viewDistances pathGrid]
--           , text "Longest path:"
--           , pre [] [text <| DistanceGrid.viewDistances longGrid]
           ]

-- returns available maze algorithms for the maze shape
algorithms : Shape -> List AlgAttr
algorithms shape =
    let algs = [ {alg = NoOp, name = algToString NoOp}]
        rectAlgs = [
            {alg = BinaryTree, name = algToString BinaryTree}
            , {alg = Sidewinder, name = algToString Sidewinder}
            , {alg = HuntAndKill, name = algToString HuntAndKill}
        ]
        allAlgs = [
            {alg = AldousBroder, name = algToString AldousBroder}
            , {alg = Wilsons, name = algToString Wilsons}
            , {alg = RecursiveBacktracker, name = algToString RecursiveBacktracker}
        ]
    in
       case shape of
           Polar -> List.concat [algs, allAlgs]
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

