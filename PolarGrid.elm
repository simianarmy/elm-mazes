-- Polar Grid module
module PolarGrid where

import Grid exposing (..)
import Mask exposing (Mask)
import Cell exposing (Cell)

import Array
import Graphics.Element as GE
import Graphics.Collage as GC
import Html
import Color

-- Does all the work of initializing a polar grid's cells
makeCells : Mask -> List Cell
makeCells mask =
    let nrows = mask.rows
        ncols = mask.cols
        rowHeight = 1 / (round nrows)
        -- rows = 2-D array of Cell that we can convet to list format on return
        rows = Array.initialize nrows Array.empty
        rows' = Array.set 0 (Array.fromList [Cell.createCell 0 0]) rows

        makeCellRows res row =
            if row >= nrows
               then res
               else
               let radius = row / nrows
                   circumference = 2 * pi * radius
                   prevCount = Array.length (Array.get (row - 1) res)
                   estCellWidth = circumference / prevCount
                   ratio = round (estCellWidth / rowHeight)
                   ncells = prevCount * ratio
                   rowCells = Array.initialize ncells (\a -> Cell.createCell row a)
                   res' = Array.set row rowCells res
               in
                  makeCellRows res' (row + 1)

        -- populate the 2D array
        acells = makeCellRows rows' 1
    in
       -- convert to list of lists and
       -- flatten inner lists to final list
       Array.map Array.toList acells
       |> Array.toList
       |> List.concat

clockwiseCell : Grid a -> Cell -> Maybe Cell
clockwiseCell grid cell =
    cell

counterClockwiseCell : Grid a -> Cell -> Maybe Cell
counterClockwiseCell grid cell =
    cell

inwardCell : Grid a -> Cell -> Maybe Cell
inwardCell grid cell =
    cell

outwardCell : Grid a -> Cell -> Maybe Cell
outwardCell grid cell =
    cell

randomCell: Grid a -> Cell
randomCell grid =
    let grid' = updateRnd grid
        randRow = grid'.rnd.row
        rowLen = List.length <| Grid.rowCells grid randRow
        -- col is rand(grid[row].length), but that reqires
        -- a dynamic rng, so this is a hack
        randCol = min rowLen grid'.rnd.col
    in
        toValidCell <| getCell grid' randRow randCol

neighbors : Grid a -> Cell -> List Cell
neighbors grid cell =
    let cw = clockwiseCell grid cell
        ccw = counterClockwiseCell grid cell
        inward = inwardCell grid cell
        outward = outwardCell grid cell
    in
       List.concat [(cellToList cw), (cellToList ccw), (cellToList inward), (cellToList outward)]


painter : Grid a -> Int -> GE.Element
painter grid cellSize =
    let imgSize = 2 * grid.rows * cellSize
        background = Color.white
        wall = Color.black
        center = (toFloat imgSize) / 2
        radius = grid.rows * cellSize
        circleForm = GC.outlined GC.defaultLine <| GC.circle (toFloat radius)

        cellLines : Cell -> List GC.Form
        cellLines cell =
            let theta = (2 * pi) / (toFloat <| List.length (Grid.rowCells grid cell.row))
                innerRadius = toFloat ((cell.row - 1) * cellSize)
                outerRadius = toFloat ((cell.row) * cellSize)
                thetaCcw = (toFloat (cell.col - 1)) * theta
                thetaCw = (toFloat (cell.col)) * theta
                ax = (center + (innerRadius * (cos thetaCcw)))
                ay = (center + (innerRadius * (sin thetaCcw)))
                bx = (center + (outerRadius * (cos thetaCcw)))
                by = (center + (outerRadius * (sin thetaCcw)))
                cx = (center + (innerRadius * (cos thetaCw)))
                cy = (center + (innerRadius * (sin thetaCw)))
                dx = (center + (outerRadius * (cos thetaCw)))
                dy = (center + (outerRadius * (sin thetaCw)))

                linkedInward = Cell.isLinked cell <| toValidCell (inwardCell grid cell)
                linkedCw  = Cell.isLinked cell <| toValidCell (clockwiseCell grid cell)
                line1 = if not linkedInward
                           then [GC.segment (ax, ay) (cx, cy)]
                           else []
                line2 = if not linkedCw
                           then [GC.segment (cx, cy) (dx, dy)]
                           else []
            in
               List.map (GC.traced GC.defaultLine) <| List.concat [line1, line2]

        drawables = List.concatMap cellLines grid.cells
        forms = circleForm :: [GC.group drawables |> GC.move (negate center, negate center)]
    in
       GC.collage (imgSize + 1) (imgSize + 1) forms
