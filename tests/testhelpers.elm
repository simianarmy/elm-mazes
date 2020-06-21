module TestHelpers exposing (..)

import Grid
import GridCell exposing (..)
import Random


unmaybeCell cell =
    GridCell.toString <| maybeGridCellToGridCell cell


createGrid rows cols =
    Grid.createGrid rows cols (Random.initialSeed 123) Grid.makeCells
