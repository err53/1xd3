module Fongf2.DraggableItem exposing (..)

import Dict exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


type alias Coord =
    ( Float, Float )


sub : Coord -> Coord -> Coord
sub ( x, y ) ( u, v ) =
    ( x - u, y - v )


add : Coord -> Coord -> Coord
add ( x, y ) ( u, v ) =
    ( x + u, y + v )


type Msg
    = Tick Float GetKeyState
    | NewCoord Coord
    | LetGo


type MouseState
    = Waiting
    | Dragging Coord



--| StopDragging


type alias Model =
    { time : Float
    , mouseState : MouseState
    , coord : Coord
    , shape : Shape Msg
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick t _ ->
            { model
                | time = t
            }

        NewCoord coord ->
            { model
                | coord = coord
                , mouseState =
                    case model.mouseState of
                        Waiting ->
                            Dragging (sub model.coord coord)

                        _ ->
                            model.mouseState
            }

        LetGo ->
            case model.mouseState of
                Dragging delta ->
                    { model
                        | coord = add model.coord delta
                        , mouseState = Waiting
                    }

                _ ->
                    model


init : Shape Msg -> Model
init shape =
    { time = 0
    , mouseState = Waiting
    , coord = ( 0, 0 )
    , shape = shape
    }


myShapes : Model -> List (Shape Msg)
myShapes model =
    let
        ( coord, notification ) =
            case model.mouseState of
                Waiting ->
                    ( model.coord
                    , notifyMouseDownAt NewCoord
                    )

                Dragging delta ->
                    ( add model.coord delta
                    , identity
                    )

        visible =
            case model.mouseState of
                Dragging _ ->
                    ( 0, 0 )

                _ ->
                    ( 100000000000, 100000000000 )
    in
    [ model.shape
        |> move coord
        |> notification
    , rect 192 128
        |> filled white
        |> makeTransparent 0
        |> move visible
        -- TODO releasing the mouse while moving does not release the circle
        |> notifyMouseMoveAt NewCoord
        |> notifyMouseUp LetGo
        |> notifyLeave LetGo
    ]


main =
    gameApp Tick { model = init (circle 20 |> filled red), view = view, update = update, title = "Game Slot" }


view : Model -> Collage Msg
view model =
    collage 192 128 (myShapes model)
