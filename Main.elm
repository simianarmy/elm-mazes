import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)

import Grid exposing (..)
import Cell

-- MODEL

type alias Model = Grid


main =
    let grid = createGrid 5 5
        cell = getCell grid 1 3
    in
        text ((gridToText grid) ++
            " cell at 1, 3 is " ++
            (cell |> Maybe.map (Cell.cellToString) |> Maybe.withDefault ""))
    -- text cellToString getCell grid
  -- start
  --   { model = 0
  --   , update = update
  --   , view = view
  --   }
