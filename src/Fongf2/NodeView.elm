module Fongf2.NodeView exposing (..)

import Dict exposing (..)
import Fongf2.Util
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


type alias Coord =
    ( Float, Float )


type Msg
    = Tick Float GetKeyState
    | NewCoord Coord
    | LetGo
    | Entered
    | Left


type MouseState
    = Waiting
    | Dragging Coord


type alias Model =
    { time : Float
    , width : Float
    , height : Float
    , mouseState : MouseState
    , coord : Coord
    , key : String
    , node : Shape Msg
    , debug : String
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick t _ ->
            { model | time = t }

        NewCoord coord ->
            let
                ( f, s ) =
                    coord
            in
            { model
                | coord = coord
                , mouseState =
                    case model.mouseState of
                        Waiting ->
                            Dragging (Fongf2.Util.sub model.coord coord)

                        _ ->
                            model.mouseState

                --   , debug = "( " ++ String.fromFloat f ++ ", " ++ String.fromFloat s ++ " )"
            }

        LetGo ->
            case model.mouseState of
                Dragging delta ->
                    let
                        ( f, s ) =
                            Fongf2.Util.add model.coord delta
                    in
                    { model
                        | coord = Fongf2.Util.add model.coord delta
                        , mouseState = Waiting

                        --   , debug = "( " ++ String.fromFloat f ++ ", " ++ String.fromFloat s ++ " )"
                    }

                _ ->
                    model

        Entered ->
            { model
                | node = renderNode True model.key
            }

        Left ->
            { model
                | node = renderNode False model.key
            }


renderNode : Bool -> String -> Shape Msg
renderNode hovering txt =
    let
        highlighting =
            if hovering then
                1

            else
                0
    in
    [ oval 24 14
        |> filled blue
        |> makeTransparent highlighting
        |> notifyEnter Entered
        |> notifyLeave Left
    , oval 20 10
        |> filled gray
    , text txt
        |> centered
        |> GraphicSVG.size 4
        |> filled black
    ]
        |> group



-- Pass in the shape that you want to make draggable


init : Float -> Float -> Coord -> String -> Shape Msg -> Model
init width height coord key node =
    { time = 0
    , width = width
    , height = height
    , mouseState = Waiting
    , coord = coord
    , key = key
    , node = node
    , debug = ""
    }


myShapes : Model -> List (Shape Msg)
myShapes model =
    let
        ( coord, notification ) =
            case model.mouseState of
                -- make a new message the first time the node is tapped on
                Waiting ->
                    ( model.coord
                    , notifyMouseDownAt NewCoord
                    )

                Dragging delta ->
                    ( Fongf2.Util.add model.coord delta
                    , identity
                    )

        visible =
            case model.mouseState of
                Dragging _ ->
                    1

                _ ->
                    0
    in
    [ model.node
        |> move coord
        |> notification
    , text model.debug
        |> GraphicSVG.size 4
        |> filled black
        |> move ( 0, 20 )
    , rect model.width model.height
        |> filled white
        |> makeTransparent 0
        |> scale visible
        -- TODO releasing the mouse while moving does not release the circle
        |> notifyMouseMoveAt NewCoord
        |> notifyMouseUp LetGo
        |> notifyLeave LetGo
    ]


view : Model -> Collage Msg
view model =
    collage model.width model.height <|
        List.concat <|
            [ myShapes model
            , [ text model.debug
                    |> GraphicSVG.size 4
                    |> alignRight
                    |> filled black
                    |> move ( 90, -62 )
              ]
            ]


main =
    gameApp Tick { model = init 192 128 ( 0, 0 ) "on god" (renderNode False "on god"), view = view, update = update, title = "Game Slot" }
