module TriangleGrid where

import Grid exposing (..)
import Mask exposing (Mask)
import Cell exposing (Cell, BaseCell, CellID, CellLinks)
import GridCell exposing (..)
import GridUtils
import Arithmetic

import Array exposing (Array)
import Graphics.Element as GE
import Graphics.Collage as GC
import Html
import Color exposing (Color)

makeCells : Mask -> CellGrid
makeCells mask =
    let createMaskedCell row col =
        if Mask.get mask row col
           then TriangleCellTag (Cell.createCell row col)
           else TriangleCellTag (Cell.createMaskedCell row col)

        makeRow row cols =
            Array.initialize mask.cols (\n -> createMaskedCell row n)

        acells = Array.initialize mask.rows (\n -> makeRow n mask.cols)
    in
        configureCells mask.rows mask.cols acells

-- no-op
configureCells : Int -> Int -> CellGrid -> CellGrid
configureCells rows cols incells =
    incells

upright : BaseCell -> Bool
upright {row, col} =
    Arithmetic.isEven (row + col)

north : Grid a -> BaseCell -> Maybe GridCell
north grid cell =
    if upright cell
       then Nothing
       else getCell grid (cell.row - 1) cell.col

south : Grid a -> BaseCell -> Maybe GridCell
south grid cell =
    if upright cell
       then getCell grid (cell.row + 1) cell.col
       else Nothing

-- east : Grid a -> BaseCell -> Maybe GridCell
-- east grid cell =
--     Grid.east grid cell
--
-- west : Grid a -> BaseCell -> Maybe GridCell
-- west grid cell =
--     Grid.west grid cell

neighbors : Grid a -> GridCell -> List GridCell
neighbors grid gc =
    case gc of
        TriangleCellTag cell ->
            GridUtils.smooshMaybes [
                west grid cell,
                east grid cell,
                north grid cell,
                south grid cell
            ]

        _ -> Debug.crash "Illegal call to HexGrid.neighbors with non-TriangleCellTag type cell"

type alias Points = {
    westX : Int,
    midX : Int,
    eastX : Int,
    apexY : Int,
    baseY : Int
}

painter :  (Grid a -> GridCell -> Color) -> Grid a -> Int -> GE.Element
painter cellPainter grid cellSize =
    let width = cellSize
        halfWidth = width / 2
        height = cellSize * (sqrt 3) / 2
        halfHeight = height / 2
        imgWidth = round(size * (grid.cols + 1) / 2)
        imgHeight = round(height * grid.rows)

        background = Color.white
        wall = Color.black

        cellBackground : GridCell -> Points -> GC.Form
        cellBackground gc vx = 
            let color = cellPainter grid gc
                ngon = GC.polygon [(vx.x_fw, vx.y_m), (vx.x_nw, vx.y_n), (vx.x_ne, vx.y_n), (vx.x_fe, vx.y_m), (vx.x_ne, vx.y_s), (vx.x_nw, vx.y_s)]
                outline = GC.solid color
            in
               GC.group [(GC.filled color ngon), (GC.outlined outline ngon)]

        maybeVisibleLine : GC.LineStyle -> (Bool, GC.Path) -> List GC.Form
        maybeVisibleLine style (visible, seg) =
            if visible
               then [GC.traced style seg]
               else []

        cellWalls : GC.LineStyle -> GridCell -> Points -> List GC.Form
        cellWalls style gc vx =
            let cell = GridCell.base gc
            in
               if cell.masked
                  then []
                  else List.concatMap (maybeVisibleLine style)
                  [
                      ((not <| GridCell.isValidCell (southwest grid cell)), (GC.segment (vx.x_fw, vx.y_m) (vx.x_nw, vx.y_s))),
                      ((not <| GridCell.isValidCell (northwest grid cell)), (GC.segment (vx.x_fw, vx.y_m) (vx.x_nw, vx.y_n))),
                      ((not <| GridCell.isValidCell (north grid cell)), (GC.segment (vx.x_nw, vx.y_n) (vx.x_ne, vx.y_n))),
                      ((not <| Cell.isLinked cell (maybeGridCellToCell (northeast grid cell))), (GC.segment (vx.x_ne, vx.y_n) (vx.x_fe, vx.y_m))),
                      ((not <| Cell.isLinked cell (maybeGridCellToCell (southeast grid cell))), (GC.segment (vx.x_fe, vx.y_m) (vx.x_ne, vx.y_s))),
                      ((not <| Cell.isLinked cell (maybeGridCellToCell (south grid cell))), (GC.segment (vx.x_ne, vx.y_s) (vx.x_nw, vx.y_s)))
                      ]

        paintCell : GridCell -> GC.Form
        paintCell gc =
            let cell = GridCell.base gc
                dl = GC.defaultLine
                style = { dl | width = 3 }
                cx = halfWidth + cell.col * halfWidth
                cy = halfHeight + cell.row * height
                points = {
                    westX = round(cx - halfWidth)
                    midX = round(cx)
                    eastX = round(cx + halfWidth)
                    apexY = if upright cell
                               then round(cy - halfHeight)
                               else round(cy + halfHeight)
                    baseY = if upright cell
                               then round(cy + halfHeight)
                               else round(cy - halfHeight)
                }
            in
               GC.group <| ((cellBackground gc points) :: (cellWalls style gc points))

        drawables = List.map paintCell (Grid.cellsList grid.cells)
        forms = [GC.group drawables |> GC.move (ox, oy)]
    in
       GC.collage (imgWidth + 1) (imgHeight + 1) forms

