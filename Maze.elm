module Maze where

import Grid exposing (..)
import DistanceGrid
import ColoredGrid
import MaskedGrid
import Mask
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
type alias Maze a = {
    grid : Grid a,
    alg : Algorithm
}

defaultAlgorithm = RecursiveBacktracker

--init : Algorithm -> Int -> Int -> Seed -> Maze a
init algType width height seed =
    let algfn = getAlgFn algType
        mask = Mask.createMask width height
        mask' = Mask.mset mask [((0, 0), False), ((2, 2), False), ((4, 4), False)]
        --grid = MaskedGrid.createGrid mask' seed
        grid = algfn <| Grid.createGrid width height seed
    in
       {
           grid = grid,
           alg = algType
       }

--update : Maze a -> Maze a
update maze =
    {maze | grid <- getAlgFn maze.alg <| Grid.update maze.grid}

--updateSize : Maze a -> Int -> Int -> Maze a
updateSize maze width height =
    init maze.alg width height (nextSeed maze.grid)

--view : Maze a -> Html
view maze =
    let root = center maze.grid
        coloredGrid = ColoredGrid.createColoredGrid maze.grid root
    in
       div [] [
           text <| (algToString maze.alg) ++ " algorithm"
           , br [] []
           , text <| (toString <| List.length (Grid.deadEnds maze.grid)) ++ " deadends"
           --, fromElement <| ColoredGrid.view coloredGrid 30
           , pre [] [ text <| Grid.toAscii Grid.cellToAscii maze.grid ]
           ]

viewDistances : Maze a -> Html
viewDistances maze =
    let --root = toValidCell <| getCell maze.grid 1 1
        root = center maze.grid
        goal = toValidCell <| getCell maze.grid maze.grid.rows 1
        dgrid = DistanceGrid.createDistanceGrid maze.grid root
        pathDistances = DistanceGrid.pathTo maze.grid root goal
        pathGrid = {dgrid | dists <- pathDistances}
        longDistances = DistanceGrid.longestPath maze.grid root
        longGrid = {dgrid | dists <- longDistances}
        rootStr = Cell.cellToString root
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
        BinaryTree -> BinaryTree.on
        Sidewinder -> Sidewinder.on
        AldousBroder -> AldousBroder.on
        Wilsons -> Wilsons.on
        HuntAndKill -> HuntAndKill.on
        RecursiveBacktracker -> RecursiveBacktracker.on

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
