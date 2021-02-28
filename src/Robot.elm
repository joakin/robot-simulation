module Robot exposing
    ( Robot, make, position
    , moveForward
    , rotateClockwise, rotateCounterclockwise
    , view
    )

{-|


# Robot

A robot that can face a direction and move forwards


## Making a robot

@docs Robot, make, position


## Moving

@docs moveForward


## Changing direction

@docs rotateClockwise, rotateCounterclockwise


## Rendering

@docs view

-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Position exposing (Position)


{-| The `Robot` type, robots have a position and are facing in a direction.

You can spawn a Robot somewhere with the `make` function.

-}
type Robot
    = Robot Position RotationDegrees Direction


type Direction
    = N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW


type alias RotationDegrees =
    Int


{-| You can spawn a Robot somewhere by passing a position. The robot will start
facing south.

    Robot.make { x = 0, y = 0 }

-}
make : Position -> Robot
make pos =
    Robot pos 0 S


rotateClockwise : Robot -> Robot
rotateClockwise (Robot pos rotation direction) =
    Robot pos (rotation + 45) <|
        case direction of
            N ->
                NE

            NE ->
                E

            E ->
                SE

            SE ->
                S

            S ->
                SW

            SW ->
                W

            W ->
                NW

            NW ->
                N


rotateCounterclockwise : Robot -> Robot
rotateCounterclockwise (Robot pos rotation direction) =
    Robot pos (rotation - 45) <|
        case direction of
            N ->
                NW

            NE ->
                N

            E ->
                NE

            SE ->
                E

            S ->
                SE

            SW ->
                S

            W ->
                SW

            NW ->
                W


{-| Using `moveForward` the robot will move in the direction it was facing and
you will get an updated robot with the new position.
-}
moveForward : Robot -> Robot
moveForward (Robot pos rotation direction) =
    let
        relativeMove =
            case direction of
                N ->
                    { x = 0, y = -1 }

                NE ->
                    { x = 1, y = -1 }

                E ->
                    { x = 1, y = 0 }

                SE ->
                    { x = 1, y = 1 }

                S ->
                    { x = 0, y = 1 }

                SW ->
                    { x = -1, y = 1 }

                W ->
                    { x = -1, y = 0 }

                NW ->
                    { x = -1, y = -1 }
    in
    Robot (Position.add pos relativeMove) rotation direction


{-| Get the position of the robot. Useful to check the position against other
data structures like a Grid.
-}
position : Robot -> Position
position (Robot pos rotation direction) =
    pos


view : Robot -> Html msg
view (Robot pos rotation direction) =
    div
        [ class "Robot"
        , style "transform" <|
            "translate(calc("
                ++ String.fromInt pos.x
                ++ " * var(--cell-size)), calc("
                ++ String.fromInt pos.y
                ++ " * var(--cell-size))) rotate("
                ++ String.fromInt rotation
                ++ "deg)"
        ]
        [ text "🤖" ]
