module Maze where

import Grid exposing (..)
import DistanceGrid exposing (..)
import ColoredGrid exposing (..)

import Cell exposing (Cell)
import BinaryTree
import Sidewinder
import AldousBroder
import Wilsons
import HuntAndKill

import Random exposing (Seed)
import Html exposing (..)
import Html.Attributes exposing (..)

type Algorithm = BinaryTree
               | Sidewinder
               | AldousBroder
               | Wilsons
               | HuntAndKill

type alias AlgAttr = {
    alg : Algorithm,
    name : String
}
type alias Maze = {
    grid : Grid {},
    alg : Algorithm
}

defaultAlgorithm = Sidewinder

init : Algorithm -> Int -> Int -> Seed -> Maze
init algType width height seed =
    let algfn = getAlgFn algType
        grid = algfn <| createGrid width height seed
    in
       {grid = grid, alg = algType}

update : Maze -> Maze
update maze =
    {maze | grid <- getAlgFn maze.alg <| Grid.update maze.grid}

updateSize : Maze -> Int -> Int -> Maze
updateSize maze width height =
    {maze | grid <- getAlgFn maze.alg <| createGrid width height (nextSeed maze.grid)}

view : Maze -> Html
view maze =
    let root = center maze.grid
        coloredGrid = createColoredGrid maze.grid root
    in
       div [] [
           text <| (algToString maze.alg) ++ " algorithm"
           , br [] []
           , text <| (toString <| List.length (Grid.deadEnds maze.grid)) ++ " deadends"
           , fromElement <| ColoredGrid.view coloredGrid 30
           ]

viewDistances : Maze -> Html
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
    ]

getAlgFn : Algorithm -> Grid {} -> Grid {}
getAlgFn algType =
    case algType of
        BinaryTree -> BinaryTree.on
        Sidewinder -> Sidewinder.on
        AldousBroder -> AldousBroder.on
        Wilsons -> Wilsons.on
        HuntAndKill -> HuntAndKill.on

algToString : Algorithm -> String
algToString algType =
    case algType of
        BinaryTree -> "Binary Tree"
        Sidewinder -> "Sidewinder"
        AldousBroder -> "Aldous-Broder"
        Wilsons -> "Wilsons"
        HuntAndKill -> "Hunt - Kill"

algByName : String -> Algorithm
algByName str =
    let res = List.head <| List.filter (\a -> a.name == str) algorithms
    in
       case res of
           Just a -> a.alg
           _ -> Sidewinder
