module Maze where

import Grid exposing (..)
import DistanceGrid
import ColoredGrid
import Mask
import PolarGrid
import Rnd
import GridCell exposing (GridCell)
import BinaryTree
import Sidewinder
import AldousBroder
import Wilsons
import HuntAndKill
import RecursiveBacktracker

import Random exposing (Seed)
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

type Display = Ascii
             | Colored
             | Polar
             | PolarAscii

type alias Maze a = {
    grid : Grid a,
    alg : Algorithm,
    display : Display
}

defaultAlgorithm = RecursiveBacktracker

cellSize : Int
cellSize = 30

-- Possible functions for creating grids:
-- generateCells

gridMaker (width, height) mask display seed =
    True

--init : Algorithm -> Int -> Int -> Seed -> Maze a
init algType width height seed display =
    let mask = Mask.createMask width height
        cellGenFn = case display of
            Polar -> PolarGrid.makeCells
            _ -> Grid.makeCells
        grid' = Grid.createGridFromMask mask seed cellGenFn
    in
       {
           grid = applyAlg algType display <| grid',
           alg = algType,
           display = display
       }

-- generates maze algorithm function taking a grid and random cell generator and
-- returning a grid
applyAlg : Algorithm -> Display -> (Grid a -> Grid a)
applyAlg algName displayType =
    let randCellFn = case displayType of
        Polar -> PolarGrid.randomCell
        _ -> Grid.randomCell
    in
        getAlgFn algName randCellFn

--update : Maze a -> Maze a
update maze =
    let grid' = Grid.update maze.grid
    in
       {
           maze | grid = applyAlg maze.alg maze.display <| grid'
       }

-- INVALIDATES MASK, SO REFRESH MAZE
--updateSize : Maze a -> Int -> Int -> Maze a
updateSize maze width height =
    init maze.alg (Debug.log "width: " width) (Debug.log "height: " height) maze.grid.rnd.seed maze.display

-- updateView : Maze a -> Display -> Maze b
updateView maze displayType =
    init maze.alg maze.grid.cols maze.grid.rows maze.grid.rnd.seed displayType

-- setMask : Maze a -> Mask -> Maze a
setMask maze mask =
    let grid' = Grid.createGridFromMask mask maze.grid.rnd.seed maze.grid.cellMaker
    in
       {maze |
           grid = applyAlg maze.alg maze.display <| grid'
       }

--view : Maze a -> Html
view maze =
    let gridHtml = case maze.display of
            Ascii ->
                pre [] [text <| Grid.toAscii maze.grid Grid.cellToAscii]
            Colored ->
                let root = Grid.center maze.grid
                    coloredGrid = ColoredGrid.createGrid maze.grid root
                in
                   fromElement <| Grid.toElement coloredGrid Grid.painter ColoredGrid.cellBackgroundColor cellSize
            PolarAscii ->
                fromElement <| Grid.toElement maze.grid PolarGrid.painter Grid.cellBackgroundColor cellSize
            Polar ->
                let (root, _) = GridCell.toPolarCell <| PolarGrid.center maze.grid
                    coloredGrid = ColoredGrid.createGrid maze.grid root
                in
                    fromElement <| Grid.toElement coloredGrid PolarGrid.painter ColoredGrid.cellBackgroundColor cellSize
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

--TODO: returns available maze algorithms for the given display type
algorithms : Display -> List AlgAttr
algorithms display =
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
       case display of
           Polar -> List.concat [algs, allAlgs]
           _ -> List.concat [algs, rectAlgs, allAlgs]

getAlgFn algType randCellFn =
    case algType of
        NoOp -> identity
        BinaryTree -> BinaryTree.on randCellFn
        Sidewinder -> Sidewinder.on randCellFn
        AldousBroder -> AldousBroder.on randCellFn
        Wilsons -> Wilsons.on randCellFn
        HuntAndKill -> HuntAndKill.on randCellFn
        RecursiveBacktracker -> RecursiveBacktracker.on randCellFn

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
    let res = List.head <| List.filter (\a -> a.name == str) (algorithms Ascii)
    in
       case res of
           Just a -> a.alg
           _ -> BinaryTree

