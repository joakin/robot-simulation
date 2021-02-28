module Robot exposing
    ( Robot, make
    , moveForward
    , rotateClockwise, rotateCounterclockwise
    )

{-|


# Robot

A robot that can face a direction and move forwards


## Making a robot

@docs Robot, make


## Moving

@docs moveForward


## Changing direction

@docs rotateClockwise, rotateCounterclockwise

-}

import Position exposing (Position)


{-| The `Robot` type, robots have a position and are facing in a direction.

You can spawn a Robot somewhere with the `make` function.

-}
type Robot
    = Robot Position Direction


type Direction
    = N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW


{-| You can spawn a Robot somewhere by passing a position. The robot will start
facing south.

    Robot.make { x = 0, y = 0 }

-}
make : Position -> Robot
make position =
    Robot position S


rotateClockwise : Robot -> Robot
rotateClockwise (Robot position direction) =
    Robot position <|
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
rotateCounterclockwise (Robot position direction) =
    Robot position <|
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
moveForward (Robot position direction) =
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
    Robot (Position.add position relativeMove) direction
