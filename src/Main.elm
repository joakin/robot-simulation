module Main exposing (main)

import Browser
import Browser.Events as Events
import Grid exposing (Grid)
import Html exposing (div, h1, node, p, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Position exposing (Position)
import Robot exposing (Robot)


type alias Flags =
    ()


type alias Model =
    { robot : Robot }


type Input
    = RotateClockwise
    | RotateCounterclockwise
    | MoveForward


type Msg
    = GotInput Input


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


grid : Grid
grid =
    Grid.make { width = 5, height = 5 }


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { robot = Robot.make (Position 2 2) }, Cmd.none )


css =
    """
:root {
    --cell-size: 50px;
}

.Grid-container {
    position: relative;
}

.Grid {
    border: 1px solid lightgray;

}

.Cell {
    border: 1px solid lightgray;
    width: var(--cell-size);
    height: var(--cell-size);
    position: absolute;
}

.Robot {
    width: var(--cell-size);
    height: var(--cell-size);
    position: absolute;
    left: 0;
    top: 0;
    font-size: calc(var(--cell-size)*0.75);
    display: flex;
    justify-content: center;
    align-items: center;
    transition: transform 100ms ease-in;
}
"""


view : Model -> Browser.Document Msg
view model =
    { title = "Robot simulation!"
    , body =
        [ Html.node "style" [] [ text css ]
        , h1 [] [ text "Robot simulation!" ]
        , p [] [ text "Instructions: Press the ⬅ and ➡ arrow keys to rotate the robot, and ⬆ to move forward." ]
        , div [ class "Grid-container" ]
            [ Grid.view grid
            , Robot.view model.robot
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ robot } as model) =
    case msg of
        GotInput input ->
            ( { model
                | robot =
                    case input of
                        RotateClockwise ->
                            Robot.rotateClockwise robot

                        RotateCounterclockwise ->
                            Robot.rotateCounterclockwise robot

                        MoveForward ->
                            let
                                newRobot =
                                    Robot.moveForward robot
                            in
                            if Grid.isValidPosition (Robot.position newRobot) grid then
                                newRobot

                            else
                                robot
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onKeyDown (keyDecoder |> Decode.map GotInput)


keyDecoder : Decoder Input
keyDecoder =
    Decode.andThen toInput (Decode.field "key" Decode.string)


toInput : String -> Decoder Input
toInput string =
    case string of
        "ArrowLeft" ->
            Decode.succeed RotateCounterclockwise

        "ArrowRight" ->
            Decode.succeed RotateClockwise

        "ArrowUp" ->
            Decode.succeed MoveForward

        key ->
            Decode.fail ("Unrecognized key press " ++ key)
