module Maze where

import Grid exposing (..)
import BinaryTree
import Sidewinder

import Random exposing (Seed)
import Html exposing (..)
import Html.Attributes exposing (..)

type Algorithm = BinaryTree | Sidewinder
type alias Maze = {
    grid : Grid,
    alg : Algorithm
}

binaryTree = BinaryTree
sidewinder = Sidewinder

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
        fromElement <| Grid.view maze.grid 20,
        text <| (algToString maze.alg) ++ " algorithm"
        ]

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
