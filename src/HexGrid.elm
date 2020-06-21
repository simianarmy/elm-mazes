-- Hex Grid module


module HexGrid exposing (..)

import Arithmetic
import Array exposing (Array)
import Cell exposing (BaseCell, Cell, CellID, CellLinks)
import Collage as GC
import Color exposing (Color)
import Element as GE
import Grid exposing (..)
import GridCell exposing (..)
import GridUtils
import Html
import Mask exposing (Mask)


makeCells : Mask -> CellGrid
makeCells mask =
    let
        createMaskedCell row col =
            if Mask.get mask row col then
                HexCellTag (Cell.createCell row col)

            else
                HexCellTag (Cell.createMaskedCell row col)

        makeRow row cols =
            Array.initialize mask.cols (\n -> createMaskedCell row n)

        acells =
            Array.initialize mask.rows (\n -> makeRow n mask.cols)
    in
    configureCells mask.rows mask.cols acells



-- no-op


configureCells : Int -> Int -> CellGrid -> CellGrid
configureCells rows cols incells =
    incells



-- return row index of a cell's north diagonal


northDiag : BaseCell -> Int
northDiag cell =
    if Arithmetic.isEven cell.col then
        cell.row - 1

    else
        cell.row



-- return row index of a cell's south diagonal


southDiag : BaseCell -> Int
southDiag cell =
    if Arithmetic.isEven cell.col then
        cell.row

    else
        cell.row + 1


northwest : Grid -> BaseCell -> Maybe GridCell
northwest grid cell =
    getCell grid (northDiag cell) (cell.col - 1)


north : Grid -> BaseCell -> Maybe GridCell
north grid cell =
    getCell grid (cell.row - 1) cell.col


northeast : Grid -> BaseCell -> Maybe GridCell
northeast grid cell =
    getCell grid (northDiag cell) (cell.col + 1)


southwest : Grid -> BaseCell -> Maybe GridCell
southwest grid cell =
    getCell grid (southDiag cell) (cell.col - 1)


south : Grid -> BaseCell -> Maybe GridCell
south grid cell =
    getCell grid (cell.row + 1) cell.col


southeast : Grid -> BaseCell -> Maybe GridCell
southeast grid cell =
    getCell grid (southDiag cell) (cell.col + 1)


getCell : Grid -> Int -> Int -> Maybe GridCell
getCell grid row col =
    Grid.getCell grid row col


neighbors : Grid -> GridCell -> List GridCell
neighbors grid gc =
    case gc of
        HexCellTag cell ->
            GridUtils.smooshMaybes
                [ northwest grid cell
                , north grid cell
                , northeast grid cell
                , southwest grid cell
                , south grid cell
                , southeast grid cell
                ]

        _ ->
            Debug.crash "Illegal call to HexGrid.neighbors with non-HexCellTag type cell"


type alias HexVertices =
    { x_fw : Float
    , x_nw : Float
    , x_ne : Float
    , x_fe : Float
    , y_n : Float
    , y_m : Float
    , y_s : Float
    }


painter : Grid -> (GridCell -> Color) -> Int -> Float -> GE.Element
painter grid cellPainter cellSize cellInset =
    let
        asize =
            toFloat cellSize / 2

        bsize =
            toFloat cellSize * sqrt 3 / 2

        width =
            cellSize * 2

        height =
            bsize * 2

        imgWidth =
            round (3 * asize * toFloat grid.cols + asize + 0.5)

        imgHeight =
            round (height * toFloat grid.rows + bsize + 0.5)

        ox =
            negate (toFloat imgWidth) / 2.0

        oy =
            negate (toFloat imgHeight) / 2.0

        background =
            Color.white

        wall =
            Color.black

        cellBackground : GridCell -> HexVertices -> GC.Form
        cellBackground gc vx =
            let
                color =
                    cellPainter gc

                ngon =
                    GC.polygon [ ( vx.x_fw, vx.y_m ), ( vx.x_nw, vx.y_n ), ( vx.x_ne, vx.y_n ), ( vx.x_fe, vx.y_m ), ( vx.x_ne, vx.y_s ), ( vx.x_nw, vx.y_s ) ]

                outline =
                    GC.solid color
            in
            GC.group [ GC.filled color ngon, GC.outlined outline ngon ]

        maybeVisibleLine : GC.LineStyle -> ( Bool, GC.Path ) -> List GC.Form
        maybeVisibleLine style ( visible, seg ) =
            if visible then
                [ GC.traced style seg ]

            else
                []

        cellWalls : GC.LineStyle -> GridCell -> HexVertices -> List GC.Form
        cellWalls style gc vx =
            let
                cell =
                    GridCell.base gc
            in
            if cell.masked then
                []

            else
                List.concatMap (maybeVisibleLine style)
                    [ ( not <| GridCell.isValidCell (southwest grid cell), GC.segment ( vx.x_fw, vx.y_m ) ( vx.x_nw, vx.y_s ) )
                    , ( not <| GridCell.isValidCell (northwest grid cell), GC.segment ( vx.x_fw, vx.y_m ) ( vx.x_nw, vx.y_n ) )
                    , ( not <| GridCell.isValidCell (north grid cell), GC.segment ( vx.x_nw, vx.y_n ) ( vx.x_ne, vx.y_n ) )
                    , ( not <| Cell.isLinked cell (maybeGridCellToCell (northeast grid cell)), GC.segment ( vx.x_ne, vx.y_n ) ( vx.x_fe, vx.y_m ) )
                    , ( not <| Cell.isLinked cell (maybeGridCellToCell (southeast grid cell)), GC.segment ( vx.x_fe, vx.y_m ) ( vx.x_ne, vx.y_s ) )
                    , ( not <| Cell.isLinked cell (maybeGridCellToCell (south grid cell)), GC.segment ( vx.x_ne, vx.y_s ) ( vx.x_nw, vx.y_s ) )
                    ]

        paintCell : GridCell -> GC.Form
        paintCell gc =
            let
                cell =
                    GridCell.base gc

                dl =
                    GC.defaultLine

                style =
                    { dl | width = 3 }

                cx =
                    cellSize + 3 * cell.col * round asize

                cy =
                    bsize + toFloat cell.row * height

                cy_ =
                    round <|
                        if Arithmetic.isOdd cell.col then
                            cy + bsize

                        else
                            cy

                -- f/n = far/near
                -- n/s/e/w = north/south/east/west
                vertices =
                    { x_fw = toFloat cx - toFloat cellSize
                    , x_nw = toFloat cx - asize
                    , x_ne = toFloat cx + asize
                    , x_fe = toFloat cx + toFloat cellSize

                    -- m = middle
                    , y_n = toFloat cy_ - bsize
                    , y_m = toFloat cy_
                    , y_s = toFloat cy_ + bsize
                    }
            in
            GC.group <| (cellBackground gc vertices :: cellWalls style gc vertices)

        drawables =
            List.map paintCell (Grid.cellsList grid.cells)

        forms =
            [ GC.group drawables |> GC.move ( ox, oy ) ]
    in
    GC.collage (imgWidth + 1) (imgHeight + 1) forms


cellBackgroundColor : Grid -> GridCell -> Color
cellBackgroundColor grid gridcell =
    Color.rgb 255 255 255
