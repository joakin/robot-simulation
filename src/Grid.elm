module Grid exposing
    ( Grid, make
    , isValidPosition
    )

{-| A grid data structure with width and height dimensions.

Grid positions are valid from 0 to width - 1 or height - 1.


# Creating a grid

@docs Grid, make


# Checking positions in the grid

@docs isValidPosition

-}

import Position exposing (Position)


type Grid
    = Grid { width : Int, height : Int }


make : { width : Int, height : Int } -> Grid
make options =
    Grid options


isValidPosition : Position -> Grid -> Bool
isValidPosition position (Grid { width, height }) =
    (position.x >= 0)
        && (position.x < width)
        && (position.y >= 0)
        && (position.y < height)
