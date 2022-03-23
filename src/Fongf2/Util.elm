module Fongf2.Util exposing (..)

import Array exposing (..)
import Dict as Dict
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import GraphicSVG.Widget as Widget
import Task
import Time


type alias Coord =
    ( Float, Float )


sub : Coord -> Coord -> Coord
sub ( x, y ) ( u, v ) =
    ( x - u, y - v )


add : Coord -> Coord -> Coord
add ( x, y ) ( u, v ) =
    ( x + u, y + v )



--! Everything below is useless and only needed
--! because the WebIDE wants the main function


type alias Model =
    { time : Float
    }


type Msg
    = Tick Float GetKeyState


init : Model
init =
    { time = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )


myShapes : Model -> List (Shape Msg)
myShapes model =
    []


view : Model -> Collage Msg
view model =
    collage 192 128 (myShapes model)


main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \_ -> ( init, Cmd.none ) -- this requests the first random number
        , update = update
        , view = \model -> { title = "Graph Theory Visualizer", body = view model }
        , subscriptions = \_ -> Sub.none
        }
