-- Weave Grid module
module WeaveGrid exposing (neighbors)

import Grid exposing (..)
import GridCell exposing (..)
import Cell exposing (..)

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
