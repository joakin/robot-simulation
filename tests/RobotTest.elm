module RobotTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Robot
import Test exposing (..)


suite : Test
suite =
    describe "Robot"
        [ test "moves south by default" <|
            \_ ->
                Robot.make { x = 1, y = 1 }
                    |> Robot.moveForward
                    |> Expect.equal (Robot.make { x = 1, y = 2 })
        , test "rotates clockwise" <|
            \_ ->
                Robot.make { x = 1, y = 1 }
                    -- Rotate twice to face West
                    |> Robot.rotateClockwise
                    |> Robot.rotateClockwise
                    |> Robot.moveForward
                    |> Expect.equal
                        (Robot.make { x = 0, y = 1 }
                            |> Robot.rotateClockwise
                            |> Robot.rotateClockwise
                        )
        , test "rotates counterclockwise" <|
            \_ ->
                Robot.make { x = 1, y = 1 }
                    -- Rotate twice to face East
                    |> Robot.rotateCounterclockwise
                    |> Robot.rotateCounterclockwise
                    |> Robot.moveForward
                    |> Expect.equal
                        (Robot.make { x = 2, y = 1 }
                            |> Robot.rotateCounterclockwise
                            |> Robot.rotateCounterclockwise
                        )
        , test "moves north" <|
            \_ ->
                Robot.make { x = 1, y = 1 }
                    -- Rotate four times to face North
                    |> Robot.rotateCounterclockwise
                    |> Robot.rotateCounterclockwise
                    |> Robot.rotateCounterclockwise
                    |> Robot.rotateCounterclockwise
                    |> Robot.moveForward
                    |> Expect.equal
                        (Robot.make { x = 1, y = 0 }
                            |> Robot.rotateCounterclockwise
                            |> Robot.rotateCounterclockwise
                            |> Robot.rotateCounterclockwise
                            |> Robot.rotateCounterclockwise
                        )
        ]
