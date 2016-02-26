-- Polar Grid module
module PolarGrid where

import Grid exposing (..)
import Mask exposing (Mask)
import Cell exposing (Cell, BaseCell, CellID, CellLinks)
import GridCell exposing (..)
import GridUtils

import Set
import Array exposing (Array)
import Graphics.Element as GE
import Graphics.Collage as GC
import Html
import Color exposing (Color)

-- Does all the work of initializing a polar grid's cells
makeCells : Mask -> CellGrid
makeCells mask =
    let nrows = mask.rows
        rowHeight = 1 / (toFloat nrows)
        -- rows = 2-D array of Cell that we can convet to list format on return
        rows = Array.initialize nrows (\r -> Array.empty)
        rows' = Array.set 0 (Array.fromList [GridCell.cellToPolarCell (Cell.createCell 0 0)]) rows

        -- row: 1..rows
        makeCellRows : CellGrid -> Int -> CellGrid
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
                   rowCells = Array.initialize ncells (\a -> 
                       GridCell.cellToPolarCell (Cell.createCell row a)
                   )
                   res' = Array.set row rowCells res
               in
                  makeCellRows res' (row + 1)

        -- populate the 2D array
        acells = makeCellRows rows' 1
    in
       configureCells nrows mask.cols acells

type alias ConfigStep = {
    cells: List GridCell,
    rows: Int,
    cols: Int
}

-- Performs additional processing on generated cells
-- Calculates each cell's parent and inward properties
configureCells : Int -> Int -> CellGrid -> CellGrid
configureCells rows cols incells =
    -- convert 2d cell grid to 1D list
    let cellList = Grid.cellsList incells
        res = {
            cells = cellList,
            rows = rows,
            cols = cols
        }
        rowLength : Int -> List GridCell -> Int
        rowLength row cells =
            List.length <| List.filter (\c -> (toRectCell c).row == row) cells

        ---- recursive worker.  accumulates results in res
        configurer : GridCell -> ConfigStep -> ConfigStep
        configurer gc work =
            -- here's what we're doing ruby-style
            -- ratio = @grid[row].length / @grid[row - 1].length
            -- parent = @grid[row - 1][col / ratio]
            -- parent.outward << cell
            -- cell.inward = parent
            let (cell, _) = Debug.log "cell: " <| GridCell.toPolarCell gc
                rowLen = Debug.log "rowLen: " <| rowLength cell.row work.cells
                divLen = Debug.log "divLen: " <| rowLength (cell.row - 1) work.cells
                ratio = Debug.log "ratio: " <| (toFloat rowLen) / (toFloat divLen)
                pcol = Debug.log "parent col: " <| floor ((toFloat cell.col) / ratio)
                -- Crash if parent is not a valid cell!
                -- TODO: LOOKUP CELL BY ROW, COL USING LIST FILTER
                parent = Debug.log "parent: " <| Grid.maybeGridCellToGridCell <| List.head <| List.filter (\c ->
                    let rc = toRectCell c
                    in
                       (rc.row == cell.row - 1 && rc.col == pcol)
                   ) work.cells
                -- update the CellLinks (outward) of this parent
                parent' = Debug.log "parent': " <| GridCell.addOutwardLink parent gc
                -- update the inward of this cell
                cell' = GridCell.setInwardCell gc parent'
                -- Replace cell with cell' in res cells
                -- Transform newCells to contain the modified parent' and cell' cells
                newCells = List.map (\c ->
                    let pcId = GridCell.id c
                    in
                       if pcId == GridCell.id parent'
                          then parent'
                          else
                          if pcId == GridCell.id cell'
                             then cell'
                             else c
                         ) work.cells
            in
               if cell.row > 0
                  then {work | cells = newCells}
                  else work

        result = List.foldl configurer res
           <| List.filter (\c -> (fst (GridCell.toPolarCell c)).row > 0) cellList
    in
       -- convert back to 2D grid
       Grid.cellsListToCellGrid result.cells

clockwiseCell : Grid a -> BaseCell -> Maybe (BaseCell, (CellID, CellLinks))
clockwiseCell grid cell =
    maybeGridCellToMaybePolarCell <| Grid.getCell grid cell.row (cell.col + 1)

counterClockwiseCell : Grid a -> (BaseCell) -> Maybe (BaseCell, (CellID, CellLinks))
counterClockwiseCell grid cell =
    maybeGridCellToMaybePolarCell <| Grid.getCell grid cell.row (cell.col - 1)

outwardCells : Grid a -> CellLinks -> List GridCell
outwardCells grid outward =
    let outwardIds = Set.toList outward
    in
        List.map (Grid.cellIdToCell grid) outwardIds

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
                outward = outwardCells grid outwardIds
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

        cellLines : (BaseCell, (CellID, CellLinks)) -> List GC.Form
        cellLines (cell, (inward, outwards)) =
            let theta = (2 * pi) / (toFloat <| List.length (Grid.rowCells grid cell.row))
                innerRadius = toFloat (cell.row * cellSize)
                outerRadius = toFloat ((cell.row + 1) * cellSize)
                thetaCcw = (toFloat cell.col) * theta
                thetaCw = (toFloat (cell.col + 1)) * theta
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

        circleForm = GC.outlined GC.defaultLine <| GC.circle (toFloat radius)
        drawables = List.concatMap cellLines <| 
            List.filter (\c -> (fst c).row > 0) (gridCellsToPolarCells (Grid.cellsList grid.cells))

        forms = circleForm :: [GC.group drawables |> GC.move (negate center, negate center)]
    in
       GC.collage (imgSize + 1) (imgSize + 1) forms
