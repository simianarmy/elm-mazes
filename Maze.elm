module Maze where

import Grid exposing (..)
import DistanceGrid
import ColoredGrid
import Mask
import MaskedGrid exposing (Masked)
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

type Algorithm = BinaryTree
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

type alias Maze a = {
    grid : Grid a,
    alg : Algorithm,
    display : Display
}

defaultAlgorithm = RecursiveBacktracker


--init : Algorithm -> Int -> Int -> Seed -> Maze a
init algType width height seed display =
    let mask = Mask.createMask width height
        grid' = MaskedGrid.createGrid mask seed 
    in
       {
           grid = getAlgFn algType <| grid',
           alg = algType,
           display = display
       }

--update : Maze a -> Maze a
update maze =
    let grid = MaskedGrid.update maze.grid
        grid' = (getAlgFn maze.alg) grid
    in
       {maze | grid = grid'}

--updateSize : Maze a -> Int -> Int -> Maze a
updateSize maze width height =
    let grid' = Grid.updateRnd maze.grid
        grid'' = {grid' | rows = height, cols = width}
    in
        {maze |
            grid = getAlgFn maze.alg <| MaskedGrid.update grid''
        }
        --{maze | grid = getAlgFn maze.alg <| createGrid width height (Rnd.nextSeed maze.grid.rnd)}

-- setMask : Maze a -> Mask -> Maze a
setMask maze mask =
    let grid = MaskedGrid.createGrid mask maze.grid.rnd.seed
    in
       {maze |
           grid = getAlgFn maze.alg <| grid
       }

--view : Maze a -> Html
view maze =
    let root = center maze.grid
        gridHtml = case maze.display of
            Ascii ->
                pre [] [text <| Grid.toAscii Grid.cellToAscii maze.grid]
            Colored ->
                let coloredGrid = ColoredGrid.createColoredGrid maze.grid root
                in
                fromElement <| ColoredGrid.view coloredGrid 30
    in
       div [] [
           text <| (algToString maze.alg) ++ " algorithm"
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
    [{alg = BinaryTree, name = algToString BinaryTree}
    , {alg = Sidewinder, name = algToString Sidewinder}
    , {alg = AldousBroder, name = algToString AldousBroder}
    , {alg = Wilsons, name = algToString Wilsons}
    , {alg = HuntAndKill, name = algToString HuntAndKill}
    , {alg = RecursiveBacktracker, name = algToString RecursiveBacktracker}
    ]

--getAlgFn : Algorithm -> Grid a -> Grid a
getAlgFn algType =
    case algType of
        BinaryTree -> BinaryTree.on Grid.randomCell
        Sidewinder -> Sidewinder.on Grid.randomCell
        AldousBroder -> AldousBroder.on MaskedGrid.randomCell
        Wilsons -> Wilsons.on MaskedGrid.randomCell
        HuntAndKill -> HuntAndKill.on MaskedGrid.randomCell
        RecursiveBacktracker -> RecursiveBacktracker.on MaskedGrid.randomCell

algToString : Algorithm -> String
algToString algType =
    case algType of
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

