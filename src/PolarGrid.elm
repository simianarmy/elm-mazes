-- Polar Grid module


module PolarGrid exposing (..)

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
import Rnd
import Set



-- Does all the work of initializing a polar grid's cells


makeCells : Mask -> CellGrid
makeCells mask =
    let
        nrows =
            mask.rows

        rowHeight =
            1 / toFloat nrows

        -- Create initial CellGrid representation
        rows =
            Array.initialize nrows (\r -> Array.empty)

        rows_ =
            Array.set 0 (Array.fromList [ GridCell.cellToPolarCell (Cell.createCell 0 0) ]) rows

        -- row: 1..rows
        makeCellRows : CellGrid -> Int -> CellGrid
        makeCellRows res row =
            if row >= nrows then
                res

            else
                let
                    radius =
                        toFloat row / toFloat nrows

                    circumference =
                        2 * pi * radius

                    prevCount =
                        Array.length (Maybe.withDefault Array.empty (Array.get (row - 1) res))

                    estCellWidth =
                        circumference / toFloat prevCount

                    ratio =
                        round (estCellWidth / rowHeight)

                    ncells =
                        prevCount * ratio

                    rowCells =
                        Array.initialize ncells
                            (\a ->
                                GridCell.cellToPolarCell (Cell.createCell row a)
                            )

                    res_ =
                        Array.set row rowCells res
                in
                makeCellRows res_ (row + 1)

        -- populate the 2D array
        acells =
            makeCellRows rows_ 1
    in
    configureCells nrows mask.cols acells



-- Work data structure for configureCells


type alias ConfigStep =
    { cells : List GridCell
    , rows : Int
    , cols : Int
    }



-- Performs additional processing on generated cells
-- Calculates each cell's parent and inward properties


configureCells : Int -> Int -> CellGrid -> CellGrid
configureCells rows cols incells =
    -- convert 2d cell grid to 1D list
    let
        cellList =
            Grid.cellsList incells

        res =
            { cells = cellList
            , rows = rows
            , cols = cols
            }

        -- necessary since we can't pass grid objects to Grid yet
        rowLength : Int -> List GridCell -> Int
        rowLength row cells =
            List.length <| List.filter (\c -> (GridCell.toRectCell c).row == row) cells

        ---- recursive worker.  accumulates results in work
        configurer : GridCell -> ConfigStep -> ConfigStep
        configurer gc work =
            -- here's what we're doing ruby-style
            -- ratio = @grid[row].length / @grid[row - 1].length
            -- parent = @grid[row - 1][col / ratio]
            -- parent.outward << cell
            -- cell.inward = parent
            let
                ( cell, _ ) =
                    GridCell.toPolarCell gc

                rowLen =
                    rowLength cell.row work.cells

                divLen =
                    rowLength (cell.row - 1) work.cells

                ratio =
                    toFloat rowLen / toFloat divLen

                pcol =
                    floor (toFloat cell.col / ratio)

                -- Crash if parent is not a valid cell!
                parent =
                    GridCell.maybeGridCellToGridCell <|
                        List.head <|
                            List.filter
                                (\c ->
                                    let
                                        rc =
                                            GridCell.toRectCell c
                                    in
                                    rc.row == cell.row - 1 && rc.col == pcol
                                )
                                work.cells

                -- update the CellLinks (outward) of this parent
                parent_ =
                    GridCell.addOutwardLink parent gc

                -- update the inward of this cell
                cell_ =
                    GridCell.setInwardCell gc parent_

                -- Transform newCells to contain the modified parent' and cell' cells
                newCells =
                    List.map
                        (\c ->
                            let
                                pcId =
                                    GridCell.id c
                            in
                            if pcId == GridCell.id parent_ then
                                parent_

                            else if pcId == GridCell.id cell_ then
                                cell_

                            else
                                c
                        )
                        work.cells
            in
            { work | cells = newCells }

        result =
            List.foldl configurer res <|
                List.filter (\c -> (GridCell.toRectCell c).row > 0) cellList
    in
    -- convert back to 2D grid
    Grid.cellsListToCellGrid result.cells


clockwiseCell : Grid -> BaseCell -> Maybe ( BaseCell, ( CellID, CellLinks ) )
clockwiseCell grid cell =
    maybeGridCellToMaybePolarCell <| getCell grid cell.row (cell.col + 1)


counterClockwiseCell : Grid -> BaseCell -> Maybe ( BaseCell, ( CellID, CellLinks ) )
counterClockwiseCell grid cell =
    maybeGridCellToMaybePolarCell <| getCell grid cell.row (cell.col - 1)


outwardCells : Grid -> CellLinks -> List GridCell
outwardCells grid outward =
    let
        outwardIds =
            Set.toList outward
    in
    List.map (cellIdToCell grid) outwardIds


center : Grid -> GridCell
center grid =
    getCell grid 0 0 |> maybeGridCellToGridCell


gridCellsToPolarCells : List GridCell -> List ( BaseCell, ( CellID, CellLinks ) )
gridCellsToPolarCells gridcells =
    List.map GridCell.toPolarCell gridcells


polarCellsToGridCells : List ( BaseCell, ( CellID, CellLinks ) ) -> List GridCell
polarCellsToGridCells cells =
    List.map (\c -> PolarCellTag c) cells


toValidCell : Maybe ( BaseCell, ( CellID, CellLinks ) ) -> ( BaseCell, ( CellID, CellLinks ) )
toValidCell cell =
    case cell of
        Just c ->
            c

        Nothing ->
            ( Cell.createNilCell, ( ( -1, -1 ), Set.empty ) )


toCellList : Maybe ( BaseCell, ( CellID, CellLinks ) ) -> List GridCell
toCellList cell =
    case cell of
        Nothing ->
            []

        Just cell ->
            [ PolarCellTag cell ]


maybeGridCellToMaybePolarCell : Maybe GridCell -> Maybe ( BaseCell, ( CellID, CellLinks ) )
maybeGridCellToMaybePolarCell cell =
    Maybe.map GridCell.toPolarCell cell


size : Grid -> Int
size grid =
    List.length <| cellsList grid.cells


randomCell : Grid -> Maybe GridCell
randomCell grid =
    let
        grid_ =
            updateRnd grid

        randRow =
            grid_.rnd.row

        rowLen =
            List.length <| Grid.rowCells grid randRow

        -- col is rand(grid[row].length)
        randCol =
            Rnd.randInt grid_.rnd rowLen
    in
    getCell grid_ randRow randCol


neighbors : Grid -> GridCell -> List GridCell
neighbors grid cell =
    case cell of
        PolarCellTag ( c, ( inId, outwardIds ) ) ->
            -- massage everything to be a [] or [gridcell]
            let
                cw =
                    toCellList <| clockwiseCell grid c

                ccw =
                    toCellList <| counterClockwiseCell grid c

                inward =
                    if Cell.isNilCellID inId then
                        []

                    else
                        [ cellIdToCell grid inId ]

                outward =
                    outwardCells grid outwardIds
            in
            List.append (List.concat [ cw, ccw, inward ]) outward

        _ ->
            []


painter : Grid -> (GridCell -> Color) -> Int -> Float -> GE.Element
painter grid cellPainter cellSize cellInset =
    let
        imgSize =
            2 * grid.rows * cellSize

        wall =
            Color.black

        center =
            toFloat imgSize / 2

        radius =
            grid.rows * cellSize

        cellLines : GridCell -> List GC.Form
        cellLines gc =
            let
                ( cell, ( inward, outwards ) ) =
                    toPolarCell gc

                theta =
                    (2 * pi) / (toFloat <| List.length (Grid.rowCells grid cell.row))

                innerRadius =
                    toFloat (cell.row * cellSize)

                outerRadius =
                    toFloat ((cell.row + 1) * cellSize)

                thetaCcw =
                    toFloat cell.col * theta

                midTheta =
                    (toFloat cell.col + 0.5) * theta

                thetaCw =
                    toFloat (cell.col + 1) * theta

                ax =
                    center + (innerRadius * cos thetaCcw)

                ay =
                    center + (innerRadius * sin thetaCcw)

                bx =
                    center + (outerRadius * cos thetaCcw)

                by =
                    center + (outerRadius * sin thetaCcw)

                cx =
                    center + (innerRadius * cos thetaCw)

                cy =
                    center + (innerRadius * sin thetaCw)

                dx =
                    center + (outerRadius * cos thetaCw)

                dy =
                    center + (outerRadius * sin thetaCw)

                midX =
                    center + (outerRadius * cos midTheta)

                midY =
                    center + (outerRadius * sin midTheta)

                linkedInward =
                    Cell.isLinked cell (Tuple.first <| GridCell.toPolarCell <| cellIdToCell grid inward)

                linkedCw =
                    Cell.isLinked cell (Tuple.first <| toValidCell (clockwiseCell grid cell))

                line1 =
                    if not linkedInward then
                        [ GC.segment ( ax, ay ) ( cx, cy ) ]

                    else
                        []

                line2 =
                    if not linkedCw then
                        [ GC.segment ( cx, cy ) ( dx, dy ) ]

                    else
                        []

                filled =
                    GC.filled (cellPainter gc) <| GC.polygon [ ( ax, ay ), ( bx, by ), ( midX, midY ), ( dx, dy ), ( cx, cy ) ]
            in
            filled :: (List.map (GC.traced GC.defaultLine) <| List.concat [ line1, line2 ])

        circleForm =
            GC.outlined GC.defaultLine <| GC.circle (toFloat radius)

        drawables =
            List.concatMap cellLines <|
                List.filter (\c -> (Tuple.first (toPolarCell c)).row > 0) (Grid.cellsList grid.cells)

        forms =
            circleForm :: [ GC.group drawables |> GC.move ( negate center, negate center ) ]
    in
    GC.collage (imgSize + 1) (imgSize + 1) forms
