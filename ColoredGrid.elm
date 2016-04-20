-- racist grid
module ColoredGrid where

import Distances exposing (Distances)
import DistanceGrid exposing (CellDistances)
import GridCell exposing (GridCell)
import Cell exposing (Cell)
import Grid exposing (Grid)

import Color exposing (Color, rgb)

type alias Colored a = {
    a |
        grid : CellDistances a,
        maximum : Int
    }

createGrid : Grid a -> GridCell -> Colored a
createGrid grid root =
    let grid' = DistanceGrid.createGrid grid root
        (farthest, max) = Distances.max grid'.dists
    in
       {
           grid = grid',
           maximum = max
       }


cellBackgroundColor : Colored a -> GridCell -> Color
cellBackgroundColor grid gridcell =
    let cell = GridCell.toRectCell gridcell
        distance = Distances.lookup grid.dists cell
        intensity = toFloat (grid.maximum - distance) / (toFloat grid.maximum)
        dark = round (255 * intensity)
        bright = round (128 + (127 * intensity))
    in
       if distance < 0
          then Color.rgb 255 255 255
          else Color.rgb dark bright dark




