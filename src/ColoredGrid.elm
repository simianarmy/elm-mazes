-- racist grid


module ColoredGrid exposing (cellBackgroundColor, createGrid)

import Cell exposing (Cell)
import Color exposing (Color, rgb)
import DistanceGrid exposing (CellDistances)
import Distances exposing (Distances)
import Grid exposing (Grid)
import GridCell exposing (GridCell)


type alias Colored =
    { dgrid : CellDistances
    , maximum : Int
    }


createGrid : Grid -> GridCell -> Colored
createGrid grid root =
    let
        dgrid =
            DistanceGrid.createGrid grid root

        ( farthest, max ) =
            Distances.max dgrid.dists
    in
    { dgrid = dgrid
    , maximum = max
    }


cellBackgroundColor : Colored -> GridCell -> Color
cellBackgroundColor cgrid gridcell =
    let
        cell =
            GridCell.toRectCell gridcell

        distance =
            Distances.lookup cgrid.dgrid.dists cell

        intensity =
            toFloat (cgrid.maximum - distance) / toFloat cgrid.maximum

        dark =
            round (255 * intensity)

        bright =
            round (128 + (127 * intensity))
    in
    if cell.tag == "DEADEND" then
        Color.lightRed

    else if distance < 0 then
        Color.rgb 255 255 255

    else
        Color.rgb dark bright dark
