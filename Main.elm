import String
import List
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)

import Grid exposing (..)
import Cell

-- MODEL

type alias Model = Grid


main =
    let grid = Grid.createGrid 3 3 
        cell = Cell.createCell 2 2
        neighs = Grid.neighbors grid cell
    in
       text ((gridToString grid) ++
           " neighbors of at 2,2 are " ++
           String.concat (List.map Cell.cellToString neighs))
       --(cell |> Maybe.map (Cell.cellToString) |> Maybe.withDefault ""))
    -- text cellToString getCell grid
  -- start
  --   { model = 0
  --   , update = update
  --   , view = view
  --   }
