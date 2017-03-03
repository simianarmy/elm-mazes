module TestHelpers where

import GridCell exposing (..)
import Grid
import Random

unmaybeCell cell =
    GridCell.toString <| maybeGridCellToGridCell cell

createGrid rows cols =
    Grid.createGrid rows cols (Random.initialSeed 123) Grid.makeCells

