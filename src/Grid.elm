module Grid exposing
    ( Grid, make
    , isValidPosition
    , view
    )

{-| A grid data structure with width and height dimensions.

Grid positions are valid from 0 to width - 1 or height - 1.


## Creating a grid

@docs Grid, make


## Checking positions in the grid

@docs isValidPosition


## Rendering a grid

@docs view

-}

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
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


view : Grid -> List (Html msg) -> Html msg
view (Grid { width, height }) children =
    div
        [ class "Grid"
        , style "min-width" ("calc(" ++ String.fromInt width ++ " * var(--cell-size))")
        , style "min-height" ("calc(" ++ String.fromInt height ++ " * var(--cell-size))")
        ]
        (List.range 0 ((width * height) - 1)
            |> List.map (viewCell width)
            |> (++) children
        )


viewCell : Int -> Int -> Html msg
viewCell width i =
    let
        row =
            i // width

        col =
            i |> remainderBy width
    in
    div
        [ class "Cell"
        , style "top" ("calc(" ++ String.fromInt row ++ " * var(--cell-size))")
        , style "left" ("calc(" ++ String.fromInt col ++ " * var(--cell-size))")
        ]
        []
