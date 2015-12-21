-- racist grid
module ColoredGrid where

import Distances exposing (Distances)
import DistanceGrid exposing (CellDistances, createDistanceGrid)
import Grid exposing (Grid, view)
import Cell exposing (Cell)
import Graphics.Element exposing (Element)
import Color exposing (Color, rgb)

import Html exposing (..)

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

--view : Colored (CellDistances (Grid a)) -> Int -> Element
view grid cellSize =
    Grid.view cellBackgroundColor grid cellSize

--cellBackgroundColor : Grid {dists: Distances, maximum: Int} -> Cell -> Color
cellBackgroundColor grid cell =
    let distance = Distances.lookup grid.dists cell
        intensity = toFloat (grid.maximum - distance) / (toFloat grid.maximum)
        dark = round (255 * intensity)
        bright = round (128 + (127 * intensity))
    in
       Color.rgb dark bright dark




