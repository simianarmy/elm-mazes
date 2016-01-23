module Maze where

import Grid exposing (..)
import DistanceGrid
import ColoredGrid
import Mask
--import PolarGrid exposing (view)
import Rnd
import Cell exposing (Cell)
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

type alias Maze a = {
    grid : Grid a,
    alg : Algorithm,
    display : Display
}

defaultAlgorithm = RecursiveBacktracker

cellSize : Int
cellSize = 20

-- Possible functions for creating grids:
-- generateCells

gridMaker (width, height) mask display seed =
    True

--init : Algorithm -> Int -> Int -> Seed -> Maze a
init algType width height seed display =
    let mask = Mask.createMask width height
        grid' = Grid.createGridFromMask mask seed 
    in
       {
           grid = getAlgFn algType <| grid', alg = algType,
           display = display
       }

--update : Maze a -> Maze a
update maze =
    let grid = Grid.update maze.grid
        grid' = (getAlgFn maze.alg) grid
    in
       {maze | grid = grid'}

-- INVALIDATES MASK, SO REFRESH MAZE
--updateSize : Maze a -> Int -> Int -> Maze a
updateSize maze width height =
    init maze.alg (Debug.log "width: " width) (Debug.log "height: " height) maze.grid.rnd.seed maze.display

-- setMask : Maze a -> Mask -> Maze a
setMask maze mask =
    let grid = Grid.createGridFromMask mask maze.grid.rnd.seed
    in
       {maze |
           grid = getAlgFn maze.alg <| grid
       }

--view : Maze a -> Html
view maze =
    let root = center maze.grid
        -- Ideally all view operations look like this:
        -- Grid.view maze.grid <Painter> cellSize
        gridHtml = case maze.display of
            Ascii ->
                pre [] [text <| Grid.toAscii Grid.cellToAscii maze.grid]
            Colored ->
                let coloredGrid = ColoredGrid.createColoredGrid maze.grid root
                in
                   fromElement <| ColoredGrid.view coloredGrid cellSize
            Polar ->
                pre [] [text <| Grid.toAscii Grid.cellToAscii maze.grid]
                --fromElement <| PolarGrid.view maze.grid cellSize
    in
       div [] [
           text <| (algToString maze.alg) ++ " algorithm"
           , br [] []
           , text <| (toString maze.grid.cols) ++ " X " ++ (toString maze.grid.rows)
           , br [] []
           , text <| (toString <| List.length (Grid.deadEnds maze.grid)) ++ " deadends"
           , gridHtml
           ]

viewDistances : Maze a -> Html
viewDistances maze =
    let --root = toValidCell <| getCell maze.grid 1 1
        root = center maze.grid
        goal = toValidCell <| getCell maze.grid maze.grid.rows 1
        --dgrid = DistanceGrid.createDistanceGrid maze.grid root
        --pathDistances = DistanceGrid.pathTo maze.grid root goal
        --pathGrid = {dgrid | dists = pathDistances}
        --longDistances = DistanceGrid.longestPath maze.grid root
        --longGrid = {dgrid | dists = longDistances}
        --rootStr = Cell.cellToString root
    in
       div [] [
          br [] [] 
           --, text <| "Cell distances from " ++ rootStr ++ ":"
           --, pre [] [text <| DistanceGrid.viewDistances dgrid]
           --, text <| "Shortest path from " ++ rootStr ++ " to SW corner:"
           --, pre [] [text <| DistanceGrid.viewDistances pathGrid]
--           , text "Longest path:"
--           , pre [] [text <| DistanceGrid.viewDistances longGrid]
           ]

--TODO: Be smarter about this
algorithms : List AlgAttr
algorithms =
    [ {alg = NoOp, name = algToString NoOp}
    , {alg = BinaryTree, name = algToString BinaryTree}
    , {alg = Sidewinder, name = algToString Sidewinder}
    , {alg = AldousBroder, name = algToString AldousBroder}
    , {alg = Wilsons, name = algToString Wilsons}
    , {alg = HuntAndKill, name = algToString HuntAndKill}
    , {alg = RecursiveBacktracker, name = algToString RecursiveBacktracker}
    ]

--getAlgFn : Algorithm -> Grid a -> Grid a
getAlgFn algType =
    case algType of
        NoOp -> identity
        BinaryTree -> BinaryTree.on Grid.randomCell
        Sidewinder -> Sidewinder.on Grid.randomCell
        AldousBroder -> AldousBroder.on Grid.randomCell
        Wilsons -> Wilsons.on Grid.randomCell
        HuntAndKill -> HuntAndKill.on Grid.randomCell
        RecursiveBacktracker -> RecursiveBacktracker.on Grid.randomCell

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
    let res = List.head <| List.filter (\a -> a.name == str) algorithms
    in
       case res of
           Just a -> a.alg
           _ -> Sidewinder

