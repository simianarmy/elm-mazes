module Grid where

import Mask exposing (Mask)
import Cell exposing (BaseCell, Cell, CellID, CellLinks)
import GridCell exposing (..)
import GridUtils

import Set exposing (Set)
import List
import ListUtils
import Array exposing (Array)
import String
import Color
import Rnd exposing (..)
import Random.PCG as Random
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Color exposing (..)
import Text exposing (..)

type alias CellGrid = Array (Array GridCell)

-- made extensible to contain additional data (ie. distances)
type alias Grid a =
    {a |
        rows: Int,
        cols: Int,
        cells: CellGrid,
        cellMaker: (Mask -> CellGrid),
        rnd: GridRnd,
        mask : Mask
}

type alias RowAscii = {
    top : String,
    bottom : String
}

-- constructor
-- createGrid : Int -> Int -> Seed -> (Mask -> CellGrid) -> Grid a
createGrid rows cols initSeed cellMaker =
    let mask' = Mask.createMask cols rows
    in createGridFromMask mask' initSeed cellMaker

--createGridFromMask : Mask -> Seed -> Grid a
createGridFromMask mask initSeed cellMaker =
    {
        rows = mask.rows,
        cols = mask.cols,
        cells = cellMaker mask,
        cellMaker = cellMaker,
        rnd = createGridRnd mask.rows mask.cols initSeed,
        mask = mask
        -- WELP, NOW I HAVE TO ADD OTHER TYPES' PROPS :(
        -- maximum = 0,
        -- dists = []
    }

-- updates all rngs with fresh seeds
updateRnd : Grid a -> Grid a
updateRnd grid =
    {grid |
        rnd = Rnd.refresh grid.rnd
    }

-- regenerates all cells based the current mask
reset grid =
    {grid |
        cells = grid.cellMaker grid.mask
    }

-- cells constructor for "basic" grids
makeCells : Mask -> CellGrid
makeCells mask =
    let createMaskedCell row col =
        if Mask.get mask row col
           then RectCellTag (Cell.createCell row col)
           else RectCellTag (Cell.createMaskedCell row col)

        makeRow row cols =
            Array.initialize mask.cols (\n -> createMaskedCell row n)
    in
       Array.initialize mask.rows (\n -> makeRow n mask.cols)

-- Returns string ASCII representation of a grid
toAscii : Grid a -> (GridCell -> String) -> String
toAscii grid cellViewer =
    let cellToString : GridCell -> RowAscii -> RowAscii
        cellToString cell ascii =
            let bcell = GridCell.base cell
                body = " " ++ (cellViewer cell) ++ " "
                east_boundary = (if Cell.isLinked bcell (maybeGridCellToCell (east grid bcell)) then " " else "|")
                south_boundary = (if Cell.isLinked bcell (maybeGridCellToCell (south grid bcell)) then "   " else "---")
                curtop = ascii.top
                curbottom = ascii.bottom
            in
               {
                   ascii |
                   top = curtop ++ body ++ east_boundary,
                   bottom = curbottom ++ south_boundary ++ "+"
               }

        rowToStrings : Int -> String
        rowToStrings row =
            let rowascii = {
                top = "|",
                bottom = "+"
                }
                cells = rowCells grid row
                finalascii = List.foldl cellToString rowascii cells
            in
               finalascii.top ++ "\n" ++ finalascii.bottom ++ "\n"
    in
       "+" ++ (String.repeat grid.cols "---+") ++ "\n" ++
       String.concat (List.map rowToStrings [0..grid.rows-1])

-- generates rectangular grid element
-- Can cellPainter be used in such a way that a ColoredGrid type does not have to be mentioned or used?
painter : Grid a -> (GridCell -> Color) -> Int -> Element
painter grid cellPainter cellSize =
    let imgWidth = cellSize * grid.cols
        imgHeight = cellSize * grid.rows
        ox = toFloat (negate imgWidth) / 2.0
        oy = toFloat imgHeight / 2.0

        maybeVisibleLine : LineStyle -> (Bool, Path) -> List Form
        maybeVisibleLine style (visible, seg) =
            if visible
               then [traced style seg]
               else []

        cellWalls : LineStyle -> GridCell -> List Form
        cellWalls style gridcell =
            let cell = GridCell.base gridcell
                x1 = toFloat (cell.col * cellSize)
                y1 = toFloat (negate cell.row * cellSize)
                x2 = toFloat ((cell.col + 1) * cellSize)
                y2 = toFloat (negate (cell.row + 1)  * cellSize)
            in
               if cell.masked
                  then []
                  else
                  List.concatMap (maybeVisibleLine style)
                  [
                      ((not <| GridCell.isValidCell (north grid cell)), (segment (x1, y1) (x2, y1))),
                      ((not <| GridCell.isValidCell (west grid cell)), (segment (x1, y1) (x1, y2))),
                      ((not <| Cell.isLinked cell (maybeGridCellToCell (east grid cell))), (segment (x2, y1) (x2, y2))),
                      ((not <| Cell.isLinked cell (maybeGridCellToCell (south grid cell))), (segment (x1, y2) (x2, y2)))
                      ]

        cellBackground : LineStyle -> GridCell -> Form
        cellBackground style cell =
            let rectcell = GridCell.base cell
                bgRect = filled (cellPainter cell) (cellToRect rectcell)
                --dbg = outlinedText style (Text.fromString " C ")
                halfSize = (toFloat cellSize) / 2.0
                cx = toFloat (rectcell.col * cellSize) + halfSize
                cy = toFloat (negate rectcell.row * cellSize) - halfSize
            in
               move (cx, cy) bgRect

        cellToRect : Cell -> Shape
        cellToRect cell =
            square <| toFloat cellSize

        paintCell : GridCell -> Form
        paintCell cell =
            let style = { defaultLine | width = 2 }
            in
               group <| ((cellBackground style cell) :: (cellWalls style cell))

        -- cast grid.cells to rectCells
        drawables = List.map paintCell (cellsList grid.cells)
    in
       collage imgWidth imgHeight [group drawables |> move (ox, oy)]

-- GRID DATA STRUCTURE CONVERSION OPERATORS
-- 1D list -> 2D array
-- 2D array -> 1D list

-- flattens 2-d array of gridcells to 1-d list
cellsList : CellGrid -> List GridCell
cellsList cells =
    List.concat <| Array.toList <| Array.map Array.toList cells

-- converts 1-d list of gridcells to 2-d array
cellsListToCellGrid : List GridCell -> CellGrid
cellsListToCellGrid cells =
    -- Determine row count
    let rowMax = Maybe.withDefault 0 <| List.maximum <| List.map GridCell.row cells
    in
       Array.initialize (rowMax + 1) (\row -> Array.fromList <| List.filter (\c -> (GridCell.row c) == row) cells)

-- apply a function to all cells
updateCells : Grid a -> (GridCell -> GridCell) -> Grid a
updateCells grid fn =
    {grid | 
        cells = List.map fn (cellsList grid.cells) |> cellsListToCellGrid
    }

-- update a single cell by id
updateCellById : Grid a -> CellID -> GridCell -> Grid a
updateCellById grid cid gc =
    let fn c =
        if GridCell.id c == cid
           then gc
           else c
    in
       updateCells grid fn

-- 0-based indices
-- returns cell at an x,y index.
-- returns nil cell if the index is invalid or the cell at that location is masked
-- TODO: CLEANUP / REFACTOR NEEDED UNLESS WE'RE OK WITH THIS FUNCTION KNOWING ABOUT 
-- ALL THE POSSIBLE GRID SHAPES.
getCell : {a | cells : CellGrid, rows : Int, cols : Int } 
    -> Int -> Int 
    -> Maybe GridCell
getCell grid row col =
    -- validate bounds
    if (row >= grid.rows || row < 0 || col < 0)
       then Nothing
       else
       let rowCells = Maybe.withDefault Array.empty <| Array.get row grid.cells
           -- Get a cell to check it's type...lame I know
           sampleCell = Array.get 0 rowCells
       in
          case sampleCell of
              Just (RectCellTag c) ->
                  if (col >= grid.cols) || c.masked
                     then Nothing
                     else Array.get col rowCells

              Just (PolarCellTag (c, o)) ->
                  -- This is ugly, but we want to recalculate col for polar grids
                  -- to remove the radial line on the right
                  let rowLen = Array.length rowCells
                     -- TODO: mask check
                  in
                      Array.get (col % rowLen) rowCells

              Just (HexCellTag c) ->
                  if (col >= grid.cols) || c.masked
                     then Nothing
                     else Array.get col rowCells

              Just (TriangleCellTag c) ->
                  if (col >= grid.cols) || c.masked
                     then Nothing
                     else Array.get col rowCells

              _ -> Nothing

getCellById : {a | cells : CellGrid, rows : Int, cols : Int } 
    -> CellID
    -> Maybe GridCell
getCellById grid cid =
    getCell grid (fst cid) (snd cid)

-- commonly used to map a maybe cell to a cell
toValidCell : Maybe Cell -> Cell
toValidCell cell =
    case cell of
        Just cell -> cell
        Nothing -> Cell.createNilCell

north : Grid a -> Cell -> Maybe GridCell
north grid cell =
    getCell grid (cell.row - 1) cell.col

south : Grid a -> Cell -> Maybe GridCell
south grid cell =
    getCell grid (cell.row + 1) cell.col

west : Grid a -> Cell -> Maybe GridCell
west grid cell =
    getCell grid cell.row (cell.col - 1)

east : Grid a -> Cell -> Maybe GridCell
east grid cell =
    getCell grid cell.row (cell.col + 1)

center : Grid a -> GridCell
center grid =
    maybeGridCellToGridCell <| getCell grid (grid.rows // 2) (grid.cols // 2)

randomCell : Grid a -> Maybe GridCell
randomCell grid =
    let (row, col) = Mask.randomLocation grid.mask grid.rnd
    in
       getCell grid row col

neighbors : Grid a -> GridCell -> List GridCell
neighbors grid cell =
    case cell of
        RectCellTag c ->
            let n = north grid c
                s = south grid c
                w = west grid c
                e = east grid c
            in
                GridUtils.smooshMaybes [n, s, w, e]

        _ -> []

-- sometimes useful to filter the neighbors of a cell by some criteria
filterNeighbors2 : (Grid a -> GridCell -> List GridCell) ->
    (GridCell -> Bool) ->
    Grid a ->
    GridCell ->
    List GridCell
filterNeighbors2 neighborsFn pred grid cell =
    List.filter pred <| neighborsFn grid cell

-- returns all cells with only 1 link
deadEnds : Grid a -> List GridCell
deadEnds grid =
    List.filter (\c -> (List.length (Set.toList (GridCell.links c)) == 1)) (cellsList grid.cells)

-- creates braids by removing deadends
-- p (0 - 1.0) controls braid factor 0 is none, 1.0 is all deadends processed
-- braid : Grid a -> 
--     -- neighbors fn
--     (Grid a -> GridCell -> List GridCell) ->
--         Float -> Grid a
braid grid neighborsFn p =
    let randpGen = Random.generate (Random.float 0 1.0)

        linkNeighbor : Grid a -> GridCell -> Grid a
        linkNeighbor g deadEnd =
            -- get all neighbors not linked to the cell
            let neighbors = filterNeighbors2 neighborsFn (\c -> not <| Cell.isLinked (GridCell.base deadEnd) (GridCell.base c)) g deadEnd
                -- select best neighbor
                best = List.filter (\c -> Set.size (GridCell.links c) == 1) neighbors
                best' = if List.isEmpty best
                           then neighbors
                           else best
                neighbor = maybeGridCellToGridCell <| GridUtils.sampleCell best' g.rnd
                g' = updateRnd g
            in
               if Cell.isNilCellID (GridCell.id neighbor)
                  then Debug.crash "NIL NEIGHBOR in braid:linkNeighbor!" g'
                  else linkCells g' deadEnd neighbor True

        processDeadEnd : GridCell -> Grid a -> Grid a
        processDeadEnd deadEnd g =
            let (randp, _) = randpGen g.rnd.seed
                g' = updateRnd g
            in
               if (randp > p) || (not <| Set.size (GridCell.links deadEnd) == 1)
                  then g'
                  else linkNeighbor g' deadEnd

        randomDeadEnds = fst <| ListUtils.shuffle (deadEnds grid) grid.rnd.seed
        grid' = updateRnd grid
    in
       List.foldl processDeadEnd grid' randomDeadEnds

-- alias to GridCell.id
gridCellID : GridCell -> CellID
gridCellID gc =
    GridCell.id gc

linkCellsHelper : Grid a -> BaseCell -> BaseCell -> Grid a
linkCellsHelper grid cell cellToLink =
    let linkCell : BaseCell -> BaseCell -> BaseCell
        linkCell cell1 cell2 = {
            cell1 |
                links = Set.insert cell2.id cell1.links
                , visited = True
        }
        linker : BaseCell -> BaseCell
        linker c =
            if c.id == cell.id
               then linkCell cell cellToLink
               else c

        linkMatched : GridCell -> GridCell
        linkMatched c =
            -- CORE FUNCTIONALITY!
            -- We are finally at the point where we must create a concrete cell object
            -- from the variable type data passed down
            case c of
                RectCellTag rc -> RectCellTag (linker rc)
                PolarCellTag (pc, data) -> PolarCellTag ((linker pc), data)
                HexCellTag rc -> HexCellTag (linker rc)
                TriangleCellTag rc -> TriangleCellTag (linker rc)

    in
       {grid | cells = cellsListToCellGrid <| List.map linkMatched (cellsList grid.cells)}

-- link 2 cells
linkCells : Grid a -> GridCell -> GridCell -> Bool -> Grid a
linkCells grid cell cell2 bidi =
    let b1 = GridCell.base <| Debug.log "Linking" cell
        b2 = GridCell.base <| Debug.log "to" cell2
        grid' = linkCellsHelper grid b1 b2
    in
       if bidi
          then linkCellsHelper grid' b2 b1
          else grid'

-- returns all cells linked to a cell
linkedCells : Grid a -> GridCell -> List GridCell
linkedCells grid cell =
    let base = GridCell.base cell
    in
        List.map (cellIdToCell grid) (Set.toList base.links)

rowMatcher : GridCell -> Int -> Bool
rowMatcher cell row =
    (GridCell.row cell) == row

-- takes 0-indexed row
rowCells : {a| cells : CellGrid} -> Int -> List GridCell
rowCells grid row =
    Array.toList <| Maybe.withDefault Array.empty <| Array.get row grid.cells

-- helper to pattern match list of unions
gridCellsToBaseCells : List GridCell -> List BaseCell
gridCellsToBaseCells gridcells =
    List.map GridCell.base gridcells

-- Not all grid types have the same # cells
size : Grid a -> Int
size grid =
    Mask.count grid.mask

-- cardinal index of a cell in a grid (1,1) = 1, etc
cellIndex : Grid a -> GridCell -> Int
cellIndex grid cell =
    let rc = GridCell.base cell
    in
       grid.cols * rc.row + rc.col

-- cardinal index of row col in a grid (0,0) = 0, etc
gridIndex : {a | cols : Int } -> Int -> Int -> Int
gridIndex grid row col =
    grid.cols * row + col

-- returns cell by its id
cellIdToCell : Grid a -> Cell.CellID -> GridCell
cellIdToCell grid cellid =
    let row = (fst cellid)
        col = (snd cellid)
    in
        maybeGridCellToGridCell <| getCell grid row col

toTitle : Grid a -> String
toTitle grid =
    Basics.toString grid.rows ++ " X " ++ Basics.toString grid.cols ++ " Grid"

cellToAscii : Grid a -> GridCell -> String
cellToAscii grid cell = 
    if (GridCell.base cell).masked
       then "M"
       else " "

cellBackgroundColor : Grid a -> GridCell -> Color
cellBackgroundColor grid cell =
    Color.white


