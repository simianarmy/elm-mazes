-- BinaryTree.elm
-- Module defining the maze Binary Tree maze creation algorithm

module BinaryTree where

import List exposing (..)
import Random exposing (..)
import GridUtils

import Grid exposing (..)
import Cell exposing (..)

on : Grid {} -> Grid {}
on grid =
    let getRandomNeighbor : Grid a -> Cell -> Maybe Cell
        getRandomNeighbor grid' cell =
            let northandeast = List.concat [
                cellToList (north grid' cell),
                cellToList (east grid' cell)]
            in
               if isEmpty northandeast
                  then Nothing
                  else
                  GridUtils.sampleCell northandeast grid'.rnd

        processCell : Cell -> Grid {} -> Grid {}
        processCell cell grid =
            let neighbor = getRandomNeighbor grid cell
                grid' = updateRnd grid
            in
               case neighbor of
                   Nothing -> grid'
                   Just neighbor -> linkCells grid' cell neighbor True
    in
       -- We want to somehow map over each cell while keeping the linking states
        List.foldl processCell grid grid.cells
