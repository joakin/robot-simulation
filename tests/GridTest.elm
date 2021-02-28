module GridTest exposing (..)

import Expect exposing (Expectation)
import Grid
import Position exposing (Position)
import Test exposing (..)


suite : Test
suite =
    describe "Grid" <|
        let
            size =
                { width = 5, height = 5 }
        in
        [ test "positions between (0,0) and (width-1,height-1) are valid" <|
            \_ ->
                expectAll
                    ([ ( 0, 0 )
                     , ( 4, 0 )
                     , ( 0, 4 )
                     , ( 4, 4 )
                     , ( 2, 3 )
                     ]
                        |> List.map
                            (\( x, y ) ->
                                Grid.make size
                                    |> Grid.isValidPosition (Position x y)
                                    |> Expect.true (String.fromInt x ++ " " ++ String.fromInt y ++ " should be a valid position")
                            )
                    )
        , test "positions outside of (0,0) and (width-1,height-1) are not valid" <|
            \_ ->
                expectAll
                    ([ ( -1, 0 )
                     , ( 0, -1 )
                     , ( 5, 0 )
                     , ( 0, 5 )
                     , ( 5, 5 )
                     , ( -1, -1 )
                     ]
                        |> List.map
                            (\( x, y ) ->
                                Grid.make size
                                    |> Grid.isValidPosition (Position x y)
                                    |> Expect.false (String.fromInt x ++ " " ++ String.fromInt y ++ " should not be a valid position")
                            )
                    )
        ]


expectAll : List Expectation -> Expectation
expectAll expectations =
    Expect.all (List.map always expectations) ()
