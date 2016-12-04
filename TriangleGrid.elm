module TriangleGrid exposing (..)

import Grid exposing (..)
import Mask exposing (Mask)
import Cell exposing (Cell, BaseCell, CellID, CellLinks)
import GridCell exposing (..)
import GridUtils
import Arithmetic

import Array exposing (Array)
import Element as GE
import Collage as GC
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
    westX : Float,
    midX : Float,
    eastX : Float,
    apexY : Float,
    baseY : Float
}

painter : Grid a -> (GridCell -> Color) -> Int -> GE.Element
painter grid cellPainter cellSize =
    let halfWidth = (toFloat cellSize) / 2
        height = (toFloat cellSize) * (sqrt 3) / 2
        halfHeight = height / 2
        imgWidth = round((toFloat cellSize) * (toFloat grid.cols + 1) / 2)
        imgHeight = round(height * toFloat grid.rows)
        ox = negate (toFloat imgWidth) / 2.0
        oy = negate (toFloat imgHeight) / 2.0

        wallColor = Color.black

        cellBackground : GridCell -> Points -> GC.Form
        cellBackground gc vx = 
            let color = cellPainter gc
                ngon = GC.polygon [(vx.westX, vx.baseY), (vx.midX, vx.apexY), (vx.eastX, vx.baseY)]
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
                noSouth = upright cell && (not <| GridCell.isValidCell (south grid cell))
                notLinked = (not <| upright cell) && (not <| Cell.isLinked cell (maybeGridCellToCell (north grid cell)))
            in
               if cell.masked
                  then []
                  else List.concatMap (maybeVisibleLine style)
                  [
                      ((not <| GridCell.isValidCell (west grid cell)), (GC.segment (vx.westX, vx.baseY) (vx.midX, vx.apexY))),
                      ((not <| Cell.isLinked cell (maybeGridCellToCell (east grid cell))), (GC.segment (vx.eastX, vx.baseY) (vx.midX, vx.apexY))),
                      ((noSouth || notLinked), (GC.segment (vx.eastX, vx.baseY) (vx.westX, vx.baseY)))
                  ]

        paintCell : GridCell -> GC.Form
        paintCell gc =
            let cell = GridCell.base gc
                dl = GC.defaultLine
                style = { dl | width = 3, color = wallColor }
                cx = halfWidth + (toFloat cell.col) * halfWidth
                cy = halfHeight + (toFloat cell.row) * height
                points = {
                    westX = cx - halfWidth,
                    midX = cx,
                    eastX = cx + halfWidth,
                    apexY = if upright cell
                               then cy - halfHeight
                               else cy + halfHeight,
                    baseY = if upright cell
                               then cy + halfHeight
                               else cy - halfHeight
                }
            in
               GC.group <| ((cellBackground gc points) :: (cellWalls style gc points))

        drawables = List.map paintCell (Grid.cellsList grid.cells)
        forms = [GC.group drawables |> GC.move (ox, oy)]
    in
       GC.collage (imgWidth + 1) (imgHeight + 1) forms

