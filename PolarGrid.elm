-- Polar Grid module
module PolarGrid where

import Grid exposing (..)
import Cell exposing (Cell)

import Graphics.Element as GE
import Graphics.Collage as GC
import Html
import Color

createGrid rows initSeed =
    Grid.createGrid rows 1 initSeed

view : Grid a -> Int -> GE.Element
view grid cellSize =
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
                linkedNorth = Cell.isLinked cell <| toValidCell (north grid cell)
                linkedEast  = Cell.isLinked cell <| toValidCell (east grid cell)
                line1 = if not linkedNorth
                           then [GC.segment (ax, ay) (cx, cy)]
                           else []
                line2 = if not linkedEast
                           then [GC.segment (cx, cy) (dx, dy)]
                           else []
            in
               List.map (GC.traced GC.defaultLine) <| List.concat [line1, line2]

        drawables = List.concatMap cellLines grid.cells
        forms = circleForm :: [GC.group drawables |> GC.move (negate center, negate center)]
    in
       GC.collage (imgSize + 1) (imgSize + 1) forms
