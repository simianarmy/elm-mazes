-- Grid with mask
module MaskedGrid where

import Mask
import Grid exposing (..)

type alias Masked a = {
    a|
        mask : Mask
    }

createGrid : Grid {} -> Masked Grid {}
createGrid grid =
    {grid |
        masks = Mask.createMask grid.rows grid.cols grid.rnd
    }
