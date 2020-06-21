module BinaryTreeTests exposing (..)

-- binarytreetests.elm

import Array
import BinaryTree
import Cell exposing (..)
import Expect
import Grid exposing (..)
import GridCell exposing (..)
import Random
import Set
import Test exposing (..)
import TestHelpers exposing (..)


generate : Grid -> Grid
generate grid =
    BinaryTree.on Grid.randomCell Grid.neighbors grid


all : Test
all =
    describe "Binary Tree Algorithm test suite"
        [ test "Grid cells are of equal length" <|
            \() ->
                let
                    seed =
                        Random.initialSeed 123123

                    grid =
                        TestHelpers.createGrid 3 3
                in
                Expect.equal (List.length (Grid.cellsList grid.cells)) (List.length (Grid.cellsList (generate grid).cells))
        , test "Cell top row links exist" <|
            \() ->
                let
                    grid =
                        TestHelpers.createGrid 3 3

                    gc00 =
                        maybeGridCellToGridCell <| Grid.getCell grid 0 0

                    gc01 =
                        maybeGridCellToGridCell <| Grid.getCell grid 0 1
                in
                -- top row cells should all have an eastern link
                Expect.true "failed"
                    (Grid.isLinkedTo gc00 (Grid.getCell grid 0 1)
                        && Grid.isLinkedTo gc01 (Grid.getCell grid 0 2)
                    )
        , test "Cell east edge links exist" <|
            \() ->
                let
                    grid =
                        TestHelpers.createGrid 3 3

                    gc20 =
                        maybeGridCellToGridCell <| Grid.getCell grid 2 0

                    gc21 =
                        maybeGridCellToGridCell <| Grid.getCell grid 2 1
                in
                -- east row cells should all have an southern link
                Expect.true "failed"
                    (Grid.isLinkedTo gc20 (Grid.getCell grid 2 1)
                        && Grid.isLinkedTo gc21 (Grid.getCell grid 2 2)
                    )
        ]
