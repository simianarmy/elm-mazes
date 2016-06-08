module TestHelpers where

import GridCell exposing (..)
import Grid
import Random.PCG

unmaybeCell cell =
    GridCell.toString <| maybeGridCellToGridCell cell

createGrid rows cols =
    Grid.createGrid rows cols (Random.PCG.initialSeed 123) Grid.makeCells

