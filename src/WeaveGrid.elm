-- Weave Grid module
module WeaveGrid exposing (neighbors, linkCells)

import Grid exposing (..)
import GridCell exposing (..)
import Cell exposing (..)

-- TODO:
-- WeaveGrids require maintaining a list of 'under' cells.
-- Figure out how to keep track of these without adding an extra property to Grid


neighbors : Grid -> GridCell -> List GridCell
neighbors grid gc =
    case gc of
        OuterCellTag c ->
            let checkNeighbor : Bool -> (Grid -> Cell -> Maybe GridCell) -> List GridCell
                checkNeighbor checkFn polarFn =
                if checkFn
                   then
                   case polarFn grid (GridCell.base gc) of
                       Just c -> [gc]
                       Nothing -> []
                   else []
            in
            List.concat [
                Grid.neighbors grid gc,
                (checkNeighbor (canTunnelNorth grid gc) Grid.north),
                (checkNeighbor (canTunnelSouth grid gc) Grid.south),
                (checkNeighbor (canTunnelEast grid gc) Grid.east),
                (checkNeighbor (canTunnelWest grid gc) Grid.west)
                ]
        _ -> []

canTunnelNorth : Grid -> GridCell -> Bool
canTunnelNorth grid gc =
    case north grid (GridCell.base gc) of
        Just northgc ->
            case north grid (GridCell.base northgc) of
                Just nngc ->
                    isHorizontalPassage grid nngc
                Nothing -> False
        Nothing -> False

canTunnelSouth : Grid -> GridCell -> Bool
canTunnelSouth grid gc =
    let cell = GridCell.base gc
    in
       case south grid cell of
           Just s ->
               case south grid (GridCell.base s) of
                   Just ss ->
                       isHorizontalPassage grid ss
                   Nothing -> False
           Nothing -> False

canTunnelEast : Grid -> GridCell -> Bool
canTunnelEast grid gc =
    case east grid (GridCell.base gc) of
        Just e ->
            case east grid (GridCell.base e) of
                Just ee ->
                    isVerticalPassage grid ee
                Nothing -> False
        Nothing -> False

canTunnelWest : Grid -> GridCell -> Bool
canTunnelWest grid gc =
    case west grid (GridCell.base gc) of
        Just w ->
            case west grid (GridCell.base w) of
                Just ww ->
                    isVerticalPassage grid ww
                Nothing -> False
        Nothing -> False

isHorizontalPassage : Grid -> GridCell -> Bool
isHorizontalPassage grid gc =
    let c = GridCell.base gc
    in
       (Grid.isLinkedTo gc (east grid c)) &&
       (Grid.isLinkedTo gc (west grid c)) &&
       (not <| Grid.isLinkedTo gc (north grid c)) &&
       (not <| Grid.isLinkedTo gc (south grid c))

isVerticalPassage : Grid -> GridCell -> Bool
isVerticalPassage grid gc =
    let c = GridCell.base gc
    in
       (Grid.isLinkedTo gc (north grid c)) &&
       (Grid.isLinkedTo gc (south grid c)) &&
       (not <| Grid.isLinkedTo gc (east grid c)) &&
       (not <| Grid.isLinkedTo gc (west grid c))

-- link 2 cells in a grid and return the modified grid
linkCells : Grid -> GridCell -> GridCell -> Bool -> Grid
linkCells grid cell cell2 bidi =
    let bc1 = GridCell.base cell
        bc2 = GridCell.base cell2
        -- Compares two cells.  if equal, returns first
        checkEqualCells : GridCell -> GridCell -> Maybe GridCell
        checkEqualCells c1 c2 =
            if (GridCell.toString c1) == (GridCell.toString c2)
               then Just c1
               else Nothing

        checkCommonNeighbor : (Grid -> Cell -> Maybe GridCell) -> (Grid -> Cell -> Maybe GridCell) -> Maybe GridCell
        checkCommonNeighbor dirFn1 dirFn2 =
            case dirFn1 grid bc1 of
                Just c1 -> (dirFn2 grid bc2)
                    |> Maybe.andThen (checkEqualCells c1)
                Nothing -> Nothing

        -- TODO: Look for Elm idiom for prettying this up?
        -- There must be a way...
        findTunnelCell : Maybe GridCell
        findTunnelCell =
            case checkCommonNeighbor north south of
                Just c -> Just c
                Nothing ->
                    case checkCommonNeighbor south north of
                        Just c -> Just c
                        Nothing ->
                            case checkCommonNeighbor east west of
                                Just c -> Just c
                                Nothing ->
                                    checkCommonNeighbor west east

    in
       case findTunnelCell of
           Just n -> tunnelUnder grid cell n
           -- Call regular ol' linkCells from the Grid module
           Nothing -> Grid.linkCells grid cell cell2 bidi

tunnelUnder : Grid -> GridCell -> GridCell -> Grid
tunnelUnder grid overCell underCell =
    grid
