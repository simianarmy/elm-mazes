-- BinaryTree.elm
-- Module defining the maze Binary Tree maze creation algorithm

module BinaryTree where

import List exposing (..)
import Mouse
import Signal
import Signal exposing (Signal, (<~), (~))
import Random exposing (..)

import Grid exposing (..)
import Cell exposing (..)

on : Grid -> Seed -> Grid
on grid seed =
    {grid | cells <- generateLinks grid seed}

generateLinks : Grid -> Seed -> List Cell
generateLinks grid seed =
    -- just fuckin generate all the rands at once - keeping the seed updated is impossible
    let randomInts = fst (generate (list (List.length grid.cells) (int 1 2)) (seed))
        getRandomNeighbor : Cell -> Maybe Cell
        getRandomNeighbor cell =
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
                        let cellIdx = cellIndex grid cell
                            -- my silly way to index the list of random ints
                            idx = head (reverse (take cellIdx randomInts))
                        in
                           case idx of
                               Nothing -> head northandeast
                               Just idx -> head (reverse (take idx northandeast))
        processCell : Cell -> Cell
        processCell cell =
            let neighbor = getRandomNeighbor cell
            in
               case neighbor of
                   Nothing -> cell
                   Just neighbor -> fst (linkCell cell neighbor True)
    in
        List.map processCell grid.cells
