-- racist grid
module ColoredGrid exposing (createGrid, cellBackgroundColor)

import Distances exposing (Distances)
import DistanceGrid exposing (CellDistances)
import GridCell exposing (GridCell)
import Cell exposing (Cell)
import Grid exposing (Grid)

import Color exposing (Color, rgb)

type alias Colored a =
    { dgrid : CellDistances a
    , maximum : Int
    }

createGrid : Grid a -> GridCell -> Colored a
createGrid grid root =
    let dgrid = DistanceGrid.createGrid grid root
        (farthest, max) = Distances.max dgrid.dists
    in
       {
           dgrid = dgrid,
           maximum = max
       }


cellBackgroundColor : Colored a -> GridCell -> Color
cellBackgroundColor cgrid gridcell =
    let cell = GridCell.toRectCell gridcell
        distance = Distances.lookup cgrid.dgrid.dists cell
        intensity = toFloat (cgrid.maximum - distance) / (toFloat cgrid.maximum)
        dark = round (255 * intensity)
        bright = round (128 + (127 * intensity))
    in
       if cell.tag == "DEADEND"
          then Color.lightRed
          else
          if distance < 0
             then Color.rgb 255 255 255
             else Color.rgb dark bright dark




