module GridTests exposing (..)

-- gridTests.elm
import Grid exposing (..)
import Cell exposing (..)
import GridCell exposing (..)
import Set
import String
import List exposing (head)
import TestHelpers exposing (..)

import Test exposing (..)
import Expect

createRectCell row col =
    RectCellTag (Cell.createCell row col)

createGrid = TestHelpers.createGrid

all : Test
all = describe "Grid test suite"
        [ test "Grid creator" <| \() -> (Expect.equal (Grid.size (createGrid 2 2)) 4)
        , test "Cell accessor 1, 1" <| \() -> (Expect.equal (unmaybeCell (getCell (createGrid 2 3) 1 1)) "(1, 1)")
        , test "Cell accessor 1, 2" <| \() -> (Expect.equal (unmaybeCell (getCell (createGrid 2 3) 1 2)) "(1, 2)")
        , test "Cell accessor 2, 1" <| \() -> (Expect.equal (unmaybeCell (getCell (createGrid 3 3) 2 1)) "(2, 1)")
        , test "Cell accessor out of bounds" 
            <| \() -> (Expect.equal False (GridCell.isValidCell (getCell (createGrid 2 3) 3 4)))
        , test "North function returns Nothing for northernmost cells" <| \() -> (
            let grid = createGrid 3 3
            in
                Expect.equal False (GridCell.isValidCell (north grid (createCell 0 0))))
        , test "North function returns cell for all non-northernmost cells" <| \() -> (
            let grid = createGrid 3 3
            in
                Expect.equal (unmaybeCell (north grid (createCell 2 1))) "(1, 1)")
        , test "East function returns Nothing for easternmost cells" <| \() -> (
            let grid = createGrid 3 3
            in
                Expect.equal False (GridCell.isValidCell (east grid (createCell 1 3))))
        , test "East function returns cell for all non-easternnmost cells" <| \() -> (
            let grid = createGrid 3 3
            in
                Expect.equal (unmaybeCell (east grid (createCell 1 1))) "(1, 2)")
        , test "South function returns Nothing for all southernnmost cells" <| \() -> (
            let grid = createGrid 3 3
            in
                Expect.equal False (GridCell.isValidCell (south grid (createCell 3 1))))
        , test "South function returns cell for all non-southernnmost cells" <| \() -> (
            let grid = createGrid 3 3
            in
                Expect.equal (unmaybeCell (south grid (createCell 1 1))) "(2, 1)")
        , test "Linking cells bidirectionally retains linking state in cells" <| \() -> (
            let grid = createGrid 2 2
                c1 = createRectCell 0 1
                c2 = createRectCell 1 1
                grid_ = linkCells grid c1 c2 True
                oneone = maybeGridCellToCell (getCell grid_ 0 1)
            in
               Expect.true "failed" (isLinked oneone (maybeGridCellToCell (south grid_ oneone))))
        , test "Linking the same cell repeatedly maintains original links" <| \() -> (
            let grid = createGrid 3 3
                c1 = createRectCell 1 1
                c2 = createRectCell 2 1
                grid_ = linkCells grid c1 c2 True
                c1_ = Debug.log "c1_" <| maybeGridCellToGridCell (getCell grid_ 1 1)
                grid__ = linkCells grid_ c1_ (createRectCell 1 2) True
                oneone = Debug.log "1,1" <| maybeGridCellToCell (getCell grid__ 1 1)
            in
               Expect.true "failed" ((Set.member (2,1) oneone.links) && (Set.member (1,2) oneone.links)))
        , test "Linking cells in a row removes their east/west wall" <| \() -> (
            let grid = createGrid 2 2
                c1 = createRectCell 0 0
                c2 = createRectCell 0 1
                grid_ = linkCells grid c1 c2 True
                bc2 = maybeGridCellToCell <| east grid_ (GridCell.base c1)
                bc1 = maybeGridCellToCell <| west grid_ (GridCell.base c2)
            in
               Expect.true "failed" ((Cell.isLinked bc1 bc2) && (Cell.isLinked bc2 bc1)))
        , test "Neighbors returns list of neighboring cells (middle of 3x3 grid)" <| \() -> (
            let grid = createGrid 3 3
            in
                Expect.equal (List.length (neighbors grid (createRectCell 1 1))) 4)
        , test "Neighbors returns list of neighboring cells (corner of 3x3 grid)" <| \() -> (
            let grid = createGrid 3 3
            in
                Expect.equal (List.length (neighbors grid (createRectCell 2 2))) 2)
        , test "Neighbors returns list of neighboring cells (corner 2x2 grid)" <| \() -> (
            let grid = createGrid 2 2
            in
                Expect.equal (List.length (neighbors grid (createRectCell 1 1))) 2)
        , test "Neighbors returns list of neighboring cells (corner 1x2 grid)" <| \() -> (
            let grid = createGrid 1 2
            in
                Expect.equal (List.length (neighbors grid (createRectCell 1 1))) 1)
        , test "Neighbors returns list of neighboring cells (corner 1x1 grid)" <| \() -> (
            let grid = createGrid 1 1
            in
                Expect.equal (List.length (neighbors grid (createRectCell 1 1))) 0)
        , test "Grid size" <| \() -> (Expect.equal (Grid.size (createGrid 3 5)) 15)
        ]

