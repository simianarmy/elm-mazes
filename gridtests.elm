-- gridTests.elm
import Grid exposing (..)
import Cell exposing (..)
import GridCell exposing (..)
import Set
import String
import List exposing (head)

import ElmTest exposing (..)
import Random.PCG exposing (..)
import Graphics.Element exposing (Element)

createGrid rows cols =
    Grid.createGrid rows cols (Random.PCG.initialSeed 123) Grid.makeCells

createRectCell row col =
    RectCellTag (Cell.createCell row col)

-- helps with getCell
unmaybeCell cell =
    GridCell.toString <| maybeGridCellToGridCell cell

gridTests : Test
gridTests = suite "Grid test suite"
        [ test "Grid creator" (assertEqual (Grid.size (createGrid 2 2)) 4)
        , test "Cell accessor 1, 1" (assertEqual (unmaybeCell (getCell (createGrid 2 3) 1 1)) "(1, 1)")
        , test "Cell accessor 1, 2" (assertEqual (unmaybeCell (getCell (createGrid 2 3) 1 2)) "(1, 2)")
        , test "Cell accessor 2, 1" (assertEqual (unmaybeCell (getCell (createGrid 3 3) 2 1)) "(2, 1)")
        , test "Cell accessor out of bounds" 
            (assertEqual False (GridCell.isValidCell (getCell (createGrid 2 3) 3 4)))
        , test "North function returns Nothing for northernmost cells" (
            let grid = createGrid 3 3
            in
                assertEqual False (GridCell.isValidCell (north grid (createCell 0 0))))
        , test "North function returns cell for all non-northernmost cells" (
            let grid = createGrid 3 3
            in
                assertEqual (unmaybeCell (north grid (createCell 2 1))) "(1, 1)")
        , test "East function returns Nothing for easternmost cells" (
            let grid = createGrid 3 3
            in
                assertEqual False (GridCell.isValidCell (east grid (createCell 1 3))))
        , test "East function returns cell for all non-easternnmost cells" (
            let grid = createGrid 3 3
            in
                assertEqual (unmaybeCell (east grid (createCell 1 1))) "(1, 2)")
        , test "South function returns Nothing for all southernnmost cells" (
            let grid = createGrid 3 3
            in
                assertEqual False (GridCell.isValidCell (south grid (createCell 3 1))))
        , test "South function returns cell for all non-southernnmost cells" (
            let grid = createGrid 3 3
            in
                assertEqual (unmaybeCell (south grid (createCell 1 1))) "(2, 1)")
        , test "Linking cells bidirectionally retains linking state in cells" (
            let grid = createGrid 2 2
                c1 = createRectCell 0 1
                c2 = createRectCell 1 1
                grid' = linkCells grid c1 c2 True
                oneone = maybeGridCellToCell (getCell grid' 0 1)
            in
               assert (isLinked oneone (maybeGridCellToCell (south grid' oneone))))
        , test "Linking the same cell repeatedly maintains original links" (
            let grid = createGrid 3 3
                c1 = createRectCell 1 1
                c2 = createRectCell 2 1
                grid' = linkCells grid c1 c2 True
                grid'' = linkCells grid' (createRectCell 1 2) (createRectCell 1 1) True
                oneone = maybeGridCellToCell (getCell grid'' 1 1)
            in
               assert ((Set.member (2,1) oneone.links) && (Set.member (1,2) oneone.links)))
        , test "Neighbors returns list of neighboring cells (middle of 3x3 grid)" (
            let grid = createGrid 3 3
            in
                assertEqual (List.length (neighbors grid (createRectCell 1 1))) 4)
        , test "Neighbors returns list of neighboring cells (corner of 3x3 grid)" (
            let grid = createGrid 3 3
            in
                assertEqual (List.length (neighbors grid (createRectCell 2 2))) 2)
        , test "Neighbors returns list of neighboring cells (corner 2x2 grid)" (
            let grid = createGrid 2 2
            in
                assertEqual (List.length (neighbors grid (createRectCell 1 1))) 2)
        , test "Neighbors returns list of neighboring cells (corner 1x2 grid)" (
            let grid = createGrid 1 2
            in
                assertEqual (List.length (neighbors grid (createRectCell 1 1))) 1)
        , test "Neighbors returns list of neighboring cells (corner 1x1 grid)" (
            let grid = createGrid 1 1
            in
                assertEqual (List.length (neighbors grid (createRectCell 1 1))) 0)
        , test "Grid size" (assertEqual (Grid.size (createGrid 3 5)) 15)
        ]

main : Element
main = elementRunner gridTests
