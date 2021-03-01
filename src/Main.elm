module Main exposing (main)

import Browser
import Browser.Events as Events
import Grid exposing (Grid)
import Html exposing (div, h1, li, node, p, section, text, ul)
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


view : Model -> Browser.Document Msg
view model =
    { title = "Robot simulation"
    , body =
        [ h1 [] [ text "Robot simulation" ]
        , section [ class "Banner" ]
            [ p [] [ text "Instructions:" ]
            , ul []
                [ li [] [ text "⬅ and ➡ arrow keys to rotate the robot" ]
                , li [] [ text "⬆ arrow key to move forward." ]
                ]
            ]
        , section [ class "Game" ]
            [ Grid.view grid [ Robot.view model.robot ] ]
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
