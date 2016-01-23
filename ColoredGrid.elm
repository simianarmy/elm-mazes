-- racist grid
module ColoredGrid where

import Distances exposing (Distances)
import DistanceGrid exposing (createDistanceGrid)

import Color exposing (Color, rgb)

type alias Colored a = {
    a |
        maximum : Int
    }

--createColoredGrid : Grid a -> Cell -> Colored (CellDistances (Grid a))
createColoredGrid grid root =
    let grid' = createDistanceGrid grid root
        (farthest, max) = Distances.max grid'.dists
    in
       {grid' | maximum = max}

--cellBackgroundColor : Grid {dists: Distances, maximum: Int} -> Cell -> Color
cellBackgroundColor grid cell =
    let distance = Distances.lookup grid.dists cell
        intensity = toFloat (grid.maximum - distance) / (toFloat grid.maximum)
        dark = round (255 * intensity)
        bright = round (128 + (127 * intensity))
    in
       Color.rgb dark bright dark




