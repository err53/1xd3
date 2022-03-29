module Fongf2.NodeView exposing (..)

import Dict exposing (..)
import Fongf2.Util
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


type alias Coord =
    ( Float, Float )


type Msg
    = Tick Float GetKeyState
    | NewNodeCoord Coord
    | AddEdge Coord
    | LetGo
    | Entered
    | Left


type MouseState
    = Waiting
    | NodeDragging Coord
    | EdgeDragging Coord


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

        NewNodeCoord coord ->
            { model
                | coord =
                    case model.mouseState of
                        EdgeDragging _ ->
                            model.coord

                        _ ->
                            coord
                , mouseState =
                    case model.mouseState of
                        Waiting ->
                            NodeDragging (Fongf2.Util.sub model.coord coord)

                        _ ->
                            model.mouseState
                , debug =
                    case model.mouseState of
                        EdgeDragging _ ->
                            "edge dragging"

                        _ ->
                            "nothing"
            }

        -- Change the mouse state to EdgeDragging when
        -- an edge is added.
        AddEdge coord ->
            { model
                | mouseState =
                    case model.mouseState of
                        Waiting ->
                            EdgeDragging coord

                        _ ->
                            model.mouseState
                , debug = "added edge"
            }

        LetGo ->
            case model.mouseState of
                NodeDragging delta ->
                    let
                        ( f, s ) =
                            Fongf2.Util.add model.coord delta
                    in
                    { model
                        | coord = Fongf2.Util.add model.coord delta
                        , mouseState = Waiting
                        , debug = "still dragging"

                        --   , debug = "( " ++ String.fromFloat f ++ ", " ++ String.fromFloat s ++ " )"
                    }

                EdgeDragging delta ->
                    { model
                        | mouseState = Waiting
                        , debug = "stopped dragging"
                    }

                _ ->
                    { model
                        | debug = "still dragging 2"
                    }

        Entered ->
            { model
                | node = renderNode True model.key
                , debug = "entered"
            }

        Left ->
            { model
                | node = renderNode False model.key
                , debug = "left"
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
        |> notifyMouseDownAt AddEdge
    , group
        [ oval 20 10
            |> filled gray
        , text txt
            |> centered
            |> GraphicSVG.size 4
            |> filled black
        ]
        |> notifyMouseDownAt NewNodeCoord
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
        coord =
            case model.mouseState of
                -- make a new message the first time the node is tapped on
                NodeDragging delta ->
                    Fongf2.Util.add model.coord delta

                -- Coordinates shouldn't change when the node isn't being
                -- dragged
                _ ->
                    model.coord

        visible =
            case model.mouseState of
                -- Make the dragging interface visible when mouseState is
                -- NodeDragging or EdgeDragging
                NodeDragging _ ->
                    1

                EdgeDragging _ ->
                    1

                _ ->
                    0
    in
    [ model.node
        |> move coord

    -- , text model.debug
    --     |> GraphicSVG.size 4
    --     |> filled black
    --     |> move ( 0, -50 )
    , rect model.width model.height
        |> filled white
        |> makeTransparent 0
        |> scale visible
        -- TODO releasing the mouse while moving does not release the circle
        |> notifyMouseMoveAt NewNodeCoord
        |> notifyMouseUp LetGo

    -- |> notifyLeave LetGo
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
