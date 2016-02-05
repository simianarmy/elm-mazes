-- BinaryTree.elm
-- Module defining the maze Binary Tree maze creation algorithm

module BinaryTree where

import List exposing (..)
import Random exposing (..)
import GridUtils

import Grid exposing (..)
import GridCell exposing (..)

on : (Grid a -> Maybe GridCell) -> Grid a -> Grid a
on startCellFn grid =
    let getRandomNeighbor : Grid a -> GridCell -> Maybe GridCell
        getRandomNeighbor grid' cell =
            let acell = Grid.toRectCell cell
                northandeast = List.concat [
                    Grid.cellToList (north grid' acell),
                    Grid.cellToList (east grid' acell)]
            in
               -- TODO: Maybe.andThen here?
               if isEmpty northandeast
                  then Nothing
                  else
                  case (GridUtils.sampleCell northandeast grid'.rnd) of
                      Nothing -> Nothing
                      Just c -> Just (RectCellTag c)

        processCell : GridCell -> Grid a -> Grid a
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
