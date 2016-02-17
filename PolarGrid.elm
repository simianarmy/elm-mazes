-- Polar Grid module
module PolarGrid where

import Grid exposing (..)
import Mask exposing (Mask)
import Cell exposing (Cell, BaseCell, CellID, CellLinks)
import GridCell exposing (..)
import GridUtils

import Set
import Array
import Graphics.Element as GE
import Graphics.Collage as GC
import Html
import Color exposing (Color)

-- Does all the work of initializing a polar grid's cells
makeCells : Mask -> List GridCell
makeCells mask =
    let nrows = mask.rows
        ncols = mask.cols
        rowHeight = 1 / (toFloat nrows)
        -- rows = 2-D array of Cell that we can convet to list format on return
        rows = Array.initialize nrows (\r -> Array.empty)
        rows' = Array.set 0 (Array.fromList [PolarCellTag ((Cell.createCell 0 0, ((-1, -1), Set.empty)))]) rows

        makeCellRows res row =
            if row >= nrows
               then res
               else
               let radius = (toFloat row) / (toFloat nrows)
                   circumference = 2 * pi * radius
                   prevCount = Array.length (Maybe.withDefault Array.empty (Array.get (row - 1) res))
                   estCellWidth = circumference / (toFloat prevCount)
                   ratio = round (estCellWidth / rowHeight)
                   ncells = prevCount * ratio
                   rowCells = Array.initialize ncells (\a -> PolarCellTag ((Cell.createCell row a), ((-1, -1), Set.empty)))
                   res' = Array.set row rowCells res
               in
                  makeCellRows res' (row + 1)

        -- populate the 2D array
        acells = makeCellRows rows' 1
        -- convert to list of lists and
        -- flatten inner lists to final list
        cellList = Array.map Array.toList acells
           |> Array.toList
           |> List.concat
    in
       configureCells nrows ncols cellList

type alias ConfigStep = {
    cells: List GridCell,
    rows: Int,
    cols: Int
}

-- Performs additional processing on generated cells
-- Calculates each cell's parent and inward properties
configureCells : Int -> Int -> List GridCell -> List GridCell
configureCells rows cols incells =
    let res = {
            cells = incells,
            rows = rows,
            cols = cols
        }
        ---- recursive worker.  accumulates results in res
        configurer : GridCell -> ConfigStep -> ConfigStep
        configurer gc work =
            let (cell, _) = GridCell.toPolarCell gc
                rowLen = List.length (Grid.rowCells res cell.row)
                divLen = List.length (Grid.rowCells res (cell.row - 1))
                ratio = (toFloat rowLen) / (toFloat divLen)
                -- parent must be a PolarCellTag
                parent = Grid.maybeGridCellToGridCell 
                <| Grid.getCell res (cell.row - 1) (cell.col // (round ratio))
                -- update the CellLinks of this PolarCellTag
                parent' = GridCell.addOutwardLink parent gc
                cell' = GridCell.setInwardCell gc parent'
                -- Replace cell with cell' in res cells
                cellIndex = GridUtils.indexOfCell cell' res.cells
                newCells = List.indexedMap (\idx -> \gcell ->
                    if (idx == cellIndex)
                       then cell'
                       else gcell
                ) res.cells
            in
               {work | cells = newCells}
    in
       -- process each cell, saving modified list as we go along
       .cells <| List.foldl configurer res incells

clockwiseCell : Grid a -> BaseCell -> Maybe (BaseCell, (CellID, CellLinks))
clockwiseCell grid cell =
    maybeGridCellToMaybePolarCell <| Grid.getCell grid cell.row (cell.col + 1)

counterClockwiseCell : Grid a -> (BaseCell) -> Maybe (BaseCell, (CellID, CellLinks))
counterClockwiseCell grid cell =
    maybeGridCellToMaybePolarCell <| Grid.getCell grid cell.row (cell.col - 1)


outwardCell : Grid a -> CellLinks -> List (BaseCell, (CellID, CellLinks))
outwardCell grid outward =
    let outwardIds = Set.toList outward
        outwardCells = List.map (Grid.cellIdToCell grid) outwardIds
    in
       gridCellsToPolarCells outwardCells

gridCellsToPolarCells : List GridCell -> List (BaseCell, (CellID, CellLinks))
gridCellsToPolarCells gridcells =
    List.map GridCell.toPolarCell gridcells

polarCellsToGridCells : List (BaseCell, (CellID, CellLinks)) -> List GridCell
polarCellsToGridCells cells =
    List.map (\c -> PolarCellTag c) cells

toValidCell : Maybe (BaseCell, (CellID, CellLinks)) -> (BaseCell, (CellID, CellLinks))
toValidCell cell =
    case cell of
        Just c -> c
        Nothing -> (Cell.createNilCell, ((-1, -1), Set.empty))

toCellList : Maybe (BaseCell, (CellID, CellLinks)) -> List GridCell
toCellList cell =
    case cell of
        Nothing -> []
        Just cell -> [PolarCellTag cell]

maybeGridCellToMaybePolarCell : Maybe GridCell -> Maybe (BaseCell, (CellID, CellLinks))
maybeGridCellToMaybePolarCell cell =
    Maybe.map GridCell.toPolarCell cell

randomCell: Grid a -> Maybe GridCell
randomCell grid =
    let grid' = updateRnd grid
        randRow = grid'.rnd.row
        rowLen = List.length <| Grid.rowCells grid randRow
        -- col is rand(grid[row].length), but that reqires
        -- a dynamic rng, so this is a hack
        randCol = min rowLen grid'.rnd.col
    in
        getCell grid' randRow randCol

neighbors : Grid a -> GridCell -> List GridCell
neighbors grid cell =
    case cell of
        PolarCellTag (c, (inId, outwardIds)) ->
            -- massage everything to be a [] or [gridcell]
            let (cw) = toCellList <| clockwiseCell grid c
                (ccw) = toCellList <| counterClockwiseCell grid c
                inward = if Cell.isNilCellID inId
                            then []
                            else [Grid.cellIdToCell grid inId]
                outward = polarCellsToGridCells <| outwardCell grid outwardIds
            in
               List.append (List.concat [cw, ccw, inward]) outward
        _ -> []

painter :  (Grid a -> GridCell -> Color) -> Grid a -> Int -> GE.Element
painter cellPainter grid cellSize =
    let imgSize = 2 * grid.rows * cellSize
        background = Color.white
        wall = Color.black
        center = (toFloat imgSize) / 2
        radius = grid.rows * cellSize
        circleForm = GC.outlined GC.defaultLine <| GC.circle (toFloat radius)

        cellLines : (BaseCell, (CellID, CellLinks)) -> List GC.Form
        cellLines (cell, (inward, outwards)) =
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

                linkedInward = Cell.isLinked cell (fst <| GridCell.toPolarCell <| (Grid.cellIdToCell grid inward))
                linkedCw  = Cell.isLinked cell (fst <| toValidCell (clockwiseCell grid cell))
                line1 = if not linkedInward
                           then [GC.segment (ax, ay) (cx, cy)]
                           else []
                line2 = if not linkedCw
                           then [GC.segment (cx, cy) (dx, dy)]
                           else []
            in
               List.map (GC.traced GC.defaultLine) <| List.concat [line1, line2]

        drawables = List.concatMap cellLines (gridCellsToPolarCells grid.cells)
        forms = circleForm :: [GC.group drawables |> GC.move (negate center, negate center)]
    in
       GC.collage (imgSize + 1) (imgSize + 1) forms
