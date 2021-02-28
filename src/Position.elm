module Position exposing
    ( Position
    , add
    )

{-| Simple wrapper for a 2d position, with x and y coordinates.

@docs Position

@docs add

-}


type alias Position =
    { x : Int, y : Int }


add : Position -> Position -> Position
add p1 p2 =
    { x = p1.x + p2.x
    , y = p1.y + p2.y
    }
