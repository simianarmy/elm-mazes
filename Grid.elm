module Grid exposing (..)

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
import Random
import Collage exposing (..)
import Element exposing (Element)
import Color exposing (..)
import Text exposing (..)

type alias CellGrid = Array (Array GridCell)

-- made extensible to contain additional data (ie. distances)
type alias Grid =
    { rows: Int,
        cols: Int,
        cells: CellGrid,
        cellMaker: (Mask -> CellGrid),
        rnd: GridRnd,
        mask : Mask,
        stack: List CellID
}

type alias RowAscii = {
    top : String,
    bottom : String
}

type alias CellCoords = {
    x1: Float,
    x2: Float,
    x3: Float,
    x4: Float,
    y1: Float,
    y2: Float,
    y3: Float,
    y4: Float
    }

-- constructor
createGrid : Int -> Int -> Random.Seed -> (Mask -> CellGrid) -> Grid
createGrid rows cols initSeed cellMaker =
    let mask_ = Mask.createMask cols rows
    in createGridFromMask mask_ initSeed cellMaker

createGridFromMask : Mask -> Random.Seed -> (Mask -> CellGrid) -> Grid
createGridFromMask mask initSeed cellMaker =
    {
        rows = mask.rows,
        cols = mask.cols,
        cells = cellMaker mask,
        cellMaker = cellMaker,
        rnd = createGridRnd mask.rows mask.cols initSeed,
        mask = mask,
        stack = []
    }

-- updates all rngs with fresh seeds
updateRnd : Grid -> Grid
updateRnd grid =
    {grid |
        rnd = Rnd.refresh grid.rnd
    }

-- regenerates all cells based the current mask
reset : Grid -> Grid
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
toAscii : Grid -> (GridCell -> String) -> String
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
       String.concat (List.map rowToStrings (List.range 0 (grid.rows - 1)))

type alias InsetCoords = {x1: Float, x2: Float, x3: Float, x4: Float, y1: Float, y2: Float, y3: Float, y4: Float}

-- generates rectangular grid element
painter : Grid -> (GridCell -> Color) -> Int -> Float -> Element
painter grid cellPainter cellSize cellInset =
    let imgWidth = cellSize * grid.cols
        imgHeight = cellSize * grid.rows
        ox = toFloat (negate imgWidth) / 2.0
        oy = toFloat imgHeight / 2.0
        inset = round ((toFloat cellSize) * cellInset)

        maybeVisibleLine : LineStyle -> (Bool, Path) -> List Form
        maybeVisibleLine style (visible, seg) =
            if visible
               then [traced style seg]
               else []

        cellCoordinatesWithInset : Int -> Int -> Int -> Int -> CellCoords
        cellCoordinatesWithInset x y cellSize inset =
            let x1 = toFloat x
                x4 = toFloat (x + cellSize)
                x2 = x1 + (toFloat inset)
                x3 = x4 - (toFloat inset)
                y1 = toFloat (negate y)
                y4 = y1 - (toFloat cellSize)
                y2 = y1 - (toFloat inset)
                y3 = y4 + (toFloat inset)
            in
                {x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                 y1 = y1, y2 = y2, y3 = y3, y4 = y4
                }

        cellWalls : LineStyle -> GridCell -> Int -> Int -> List Form
        cellWalls style gridcell x y =
            let cell = GridCell.base gridcell
                x1 = toFloat x
                y1 = toFloat (negate y)
                x2 = x1 + (toFloat cellSize)
                y2 = (y1 - toFloat cellSize)
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

        cellWallsWithInset : LineStyle -> GridCell -> InsetCoords -> List Form
        cellWallsWithInset style gridcell {x1,x2,x3,x4,y1,y2,y3,y4} =
            let cell = GridCell.base gridcell
            in
               if cell.masked
                  then []
                  else
                  List.map (traced style) <|
                  List.concat [
                      (if Cell.isLinked cell (maybeGridCellToCell (north grid cell))
                      then [segment (x2, y1) (x2, y2), segment (x3, y1) (x3, y2)]
                      else [segment (x2, y2) (x3, y2)])
                      , (if Cell.isLinked cell (maybeGridCellToCell (south grid cell))
                      then [segment (x2, y3) (x2, y4), segment (x3, y3) (x3, y4)]
                      else [segment (x2, y3) (x3, y3)])
                      , (if Cell.isLinked cell (maybeGridCellToCell (west grid cell))
                      then [segment (x1, y2) (x2, y2), segment (x1, y3) (x2, y3)]
                      else [segment (x2, y2) (x2, y3)])
                      , (if Cell.isLinked cell (maybeGridCellToCell (east grid cell))
                      then [segment (x3, y2) (x4, y2), segment (x3, y3) (x4, y3)]
                      else [segment (x3, y2) (x3, y3)])
                      ]

        -- Draws colored cell background
        cellBackground : LineStyle -> GridCell -> Form
        cellBackground style gc =
            let cell = GridCell.base gc
                bgRect = filled (cellPainter gc) (cellToRect cell)
                --dbg = outlinedText style (Text.fromString " C ")
                halfSize = (toFloat cellSize) / 2.0
                cx = toFloat (cell.col * cellSize) + halfSize
                cy = toFloat (negate cell.row * cellSize) - halfSize
            in
               move (cx, cy) bgRect

        -- Draws rectangles for each of the inset regions (up to 5)
        cellBackgroundWithInset : LineStyle -> GridCell -> InsetCoords -> Form
        cellBackgroundWithInset style gc {x1,x2,x3,x4,y1,y2,y3,y4} =
            let cell = GridCell.base gc
                halfSize = (toFloat cellSize) / 2.0
                halfInsetWidth = (x3 - x2) / 2.0
                halfInsetHeight = (y3 - y2) / 2.0
                cx = toFloat (cell.col * cellSize) + halfSize
                cy = toFloat (negate cell.row * cellSize) - halfSize
                fillfn = filled (cellPainter gc)
                middleRect = fillfn <| rect (x3 - x2) (y3 - y2)
                rects = middleRect :: List.concat [
                  (if Cell.isLinked cell (maybeGridCellToCell (north grid cell))
                  then [moveY -halfInsetHeight <| fillfn <| rect (x3 - x2) (y2 - y1)]
                  else [])
                  , (if Cell.isLinked cell (maybeGridCellToCell (south grid cell))
                  then [moveY halfInsetHeight <| fillfn <| rect (x3 - x2) (y4 - y3)]
                  else [])
                  , (if Cell.isLinked cell (maybeGridCellToCell (west grid cell))
                  then [moveX -halfInsetWidth <| fillfn <| rect (x2 - x1) (y3 - y2)]
                  else [])
                  , (if Cell.isLinked cell (maybeGridCellToCell (east grid cell))
                  then [moveX halfInsetWidth <| fillfn <| rect (x4 - x3) (y3 - y2)]
                  else [])
                  ]
            in
               move (cx, cy) <| group rects

        cellToRect : Cell -> Shape
        cellToRect cell =
            square <| toFloat cellSize

        paintCell : GridCell -> Form
        paintCell gcell =
            let style = { defaultLine | width = 2 }
                cell = GridCell.base gcell
                x = cell.col * cellSize
                y = cell.row * cellSize
                forms = if inset > 0
                           then
                           let insetCoords = cellCoordinatesWithInset x y cellSize inset
                               walls = cellWallsWithInset style gcell insetCoords
                               bg = cellBackgroundWithInset style gcell insetCoords
                           in
                              bg :: walls
                        else (cellBackground style gcell) :: (cellWalls style gcell x y)
               in
               group forms

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
updateCells : Grid -> (GridCell -> GridCell) -> Grid
updateCells grid fn =
    {grid | 
        cells = List.map fn (cellsList grid.cells) |> cellsListToCellGrid
    }

-- update a single cell by id
updateCellById : Grid -> CellID -> GridCell -> Grid
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
    getCell grid (Tuple.first cid) (Tuple.second cid)

-- commonly used to map a maybe cell to a cell
toValidCell : Maybe Cell -> Cell
toValidCell cell =
    case cell of
        Just cell -> cell
        Nothing -> Cell.createNilCell

north : Grid -> Cell -> Maybe GridCell
north grid cell =
    getCell grid (cell.row - 1) cell.col

south : Grid -> Cell -> Maybe GridCell
south grid cell =
    getCell grid (cell.row + 1) cell.col

west : Grid -> Cell -> Maybe GridCell
west grid cell =
    getCell grid cell.row (cell.col - 1)

east : Grid -> Cell -> Maybe GridCell
east grid cell =
    getCell grid cell.row (cell.col + 1)

center : Grid -> GridCell
center grid =
    maybeGridCellToGridCell <| getCell grid (grid.rows // 2) (grid.cols // 2)

randomCell : Grid -> Maybe GridCell
randomCell grid =
    let (row, col) = Mask.randomLocation grid.mask grid.rnd
    in
       getCell grid row col

neighbors : Grid -> GridCell -> List GridCell
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
filterNeighbors2 : (Grid -> GridCell -> List GridCell) ->
    (GridCell -> Bool) ->
    Grid ->
    GridCell ->
    List GridCell
filterNeighbors2 neighborsFn pred grid cell =
    List.filter pred <| neighborsFn grid cell

-- returns all cells with only 1 link
deadEnds : Grid -> List GridCell
deadEnds grid =
    List.filter (\c -> (List.length (Set.toList (GridCell.links c)) == 1)) (cellsList grid.cells)

-- creates braids by removing deadends
-- p (0 - 1.0) controls braid factor 0 is none, 1.0 is all deadends processed
braid : Grid ->
     -- neighbors fn
     (Grid -> GridCell -> List GridCell) ->
     -- braid factor
     Float ->
     Grid
braid grid neighborsFn p =
    let randpGen = Random.float 0 1.0

        linkNeighbor : Grid -> GridCell -> Grid
        linkNeighbor g deadEnd =
            -- get all neighbors not linked to the cell
            let neighbors = filterNeighbors2 neighborsFn (\c -> not <| Cell.isLinked (GridCell.base deadEnd) (GridCell.base c)) g deadEnd
                -- select best neighbor
                best = List.filter (\c -> Set.size (GridCell.links c) == 1) neighbors
                best_ = if List.isEmpty best
                           then neighbors
                           else best
                neighbor = maybeGridCellToGridCell <| GridUtils.sampleCell best_ g.rnd
                g_ = updateRnd g
            in
               if Cell.isNilCellID (GridCell.id neighbor)
                  then Debug.crash "NIL NEIGHBOR in braid:linkNeighbor!" g_
                  else linkCells g_ deadEnd neighbor True

        processDeadEnd : GridCell -> Grid -> Grid
        processDeadEnd deadEnd g =
            let (randp, _) = Random.step randpGen g.rnd.seed
                g_ = updateRnd g
            in
               if (randp > p) || (not <| Set.size (GridCell.links deadEnd) == 1)
                  then g_
                  else linkNeighbor g_ deadEnd

        randomDeadEnds = Tuple.first <| ListUtils.shuffle (deadEnds grid) grid.rnd.seed
        grid_ = updateRnd grid
    in
       List.foldl processDeadEnd grid_ randomDeadEnds

-- alias to GridCell.id
gridCellID : GridCell -> CellID
gridCellID gc =
    GridCell.id gc

linkCellsHelper : Grid -> BaseCell -> BaseCell -> Grid
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
linkCells : Grid -> GridCell -> GridCell -> Bool -> Grid
linkCells grid cell cell2 bidi =
    let b1 = GridCell.base <| Debug.log "Linking" cell
        b2 = GridCell.base <| Debug.log "to" cell2
        grid_ = linkCellsHelper grid b1 b2
    in
       if bidi
          then linkCellsHelper grid_ b2 b1
          else grid_

-- returns all cells linked to a cell
linkedCells : Grid -> GridCell -> List GridCell
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
size : Grid -> Int
size grid =
    Mask.count grid.mask

-- cardinal index of a cell in a grid (1,1) = 1, etc
cellIndex : Grid -> GridCell -> Int
cellIndex grid cell =
    let rc = GridCell.base cell
    in
       grid.cols * rc.row + rc.col

-- cardinal index of row col in a grid (0,0) = 0, etc
gridIndex : {a | cols : Int } -> Int -> Int -> Int
gridIndex grid row col =
    grid.cols * row + col

-- returns cell by its id
cellIdToCell : Grid -> Cell.CellID -> GridCell
cellIdToCell grid cellid =
    let row = (Tuple.first cellid)
        col = (Tuple.second cellid)
    in
        maybeGridCellToGridCell <| getCell grid row col

toTitle : Grid -> String
toTitle grid =
    Basics.toString grid.rows ++ " X " ++ Basics.toString grid.cols ++ " Grid"

cellToAscii : Grid -> GridCell -> String
cellToAscii grid cell = 
    if (GridCell.base cell).masked
       then "M"
       else " "

cellBackgroundColor : Grid -> GridCell -> Color
cellBackgroundColor grid cell =
    Color.white


