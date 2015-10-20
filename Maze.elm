module Maze where

import Grid exposing (..)
import Cell exposing (Cell)
import BinaryTree
import Sidewinder
import Dijkstra
import Distances exposing (Distances)

import Random exposing (Seed)
import Html exposing (..)
import Html.Attributes exposing (..)

type Algorithm = BinaryTree | Sidewinder
type alias AlgAttr = {
    alg : Algorithm,
    name : String
}
type alias Maze = {
    grid : Grid,
    alg : Algorithm
}

binaryTree = BinaryTree
sidewinder = Sidewinder

defaultAlgorithm = sidewinder

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
    div [] [
        fromElement <| Grid.view maze.grid 30,
        text <| (algToString maze.alg) ++ " algorithm"
        ]

algorithms : List AlgAttr
algorithms =
    [{alg = BinaryTree, name = "Binary Tree"}, {alg = Sidewinder, name = "Sidewinder"}]

getAlgFn : Algorithm -> Grid -> Grid
getAlgFn algType =
    case algType of
        BinaryTree -> BinaryTree.on
        Sidewinder -> Sidewinder.on

algToString : Algorithm -> String
algToString algType =
    case algType of
        BinaryTree -> "Binary Tree"
        Sidewinder -> "Sidewinder"

algByName : String -> Algorithm
algByName str =
    let res = List.head <| List.filter (\a -> a.name == str) algorithms
    in
       case res of
           Just a -> a.alg
           _ -> Sidewinder

-- Returns all distances from a root cell
distances : Maze -> Distances
distances maze =
    let rootCell = toValidCell <| Grid.getCell maze.grid 1 1
    in
        Dijkstra.cellDistances maze.grid rootCell

