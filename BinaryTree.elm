-- BinaryTree.elm
-- Module defining the maze Binary Tree maze creation algorithm

module BinaryTree where

import List exposing (..)
import Random exposing (..)

import Grid exposing (..)
import Cell exposing (..)

name : String
name =
    "Binary Tree"

on : Grid -> Grid
on grid =
    -- just fuckin generate all the rands at once - keeping the seed updated is impossible
    let randomInts = fst <| generate (list (length grid.cells) (int 1 2)) grid.rnd.seed
        getRandomNeighbor : Cell -> Int -> Maybe Cell
        getRandomNeighbor cell randInt =
            let northandeast = List.concat [
                cellToList (north grid cell),
                cellToList (east grid cell)]
            in
               if isEmpty northandeast
                  then Nothing
                  else 
                    if (length northandeast) == 1
                       then head northandeast
                       else
                       -- pick one of two
                       head (reverse (take randInt northandeast))

        processCell : (Cell, Int) -> Grid -> Grid
        processCell (cell, randInt) grid =
            let neighbor = getRandomNeighbor cell randInt
            in
               case neighbor of
                   Nothing -> grid
                   Just neighbor -> linkCells grid cell neighbor True
    in
       -- We want to somehow map over each cell while keeping the linking states
        List.foldl processCell grid (map2 (,) grid.cells randomInts)
