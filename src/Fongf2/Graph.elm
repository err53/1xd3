module Fongf2.Graph exposing (..)

import Array exposing (..)
import Dict as Dict
import Fongf2.NodeView
import Fongf2.Util
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import GraphicSVG.Widget as Widget
import Html exposing (main_)
import Task
import Time



-- Nodes and edges of graph
-- Displays the graph on the screen


type alias Node =
    { val : Fongf2.NodeView.Model
    , edges : List String
    }


type alias Graph =
    Dict.Dict String Node


type alias Model =
    { time : Float
    , width : Float
    , height : Float
    , nodes : Graph
    , draggedNode : String
    , debug : String
    }


type Msg
    = Tick Float GetKeyState
    | NodeViewMsg String Fongf2.NodeView.Msg
    | AddNode String
    | AddEdge String String



--* Get the node from the graph given a key


dictGet : String -> Graph -> Node
dictGet key graph =
    case Dict.get key graph of
        Just a ->
            a

        Nothing ->
            { val =
                Fongf2.NodeView.init 600 1024 ( 0, 0 ) "error" <|
                    Fongf2.NodeView.renderNode False "error"
            , edges = []
            }


init : Float -> Float -> Model
init width height =
    let
        node coord txt =
            Fongf2.NodeView.init width height coord txt <|
                Fongf2.NodeView.renderNode False txt
    in
    { time = 0
    , width = width
    , height = height
    , nodes =
        --Dict.empty
        Dict.fromList
            [ ( "A", { val = node ( 50, 50 ) "A", edges = [ "B", "C" ] } )
            , ( "B", { val = node ( 50, 0 ) "B", edges = [ "A" ] } )
            , ( "C", { val = node ( -25, 0 ) "C", edges = [ "D" ] } )
            , ( "D", { val = node ( -25, -25 ) "D", edges = [ "A", "C", "F", "E" ] } )
            , ( "E", { val = node ( 0, 50 ) "E", edges = [ "F", "E", "B" ] } )
            , ( "F", { val = node ( 0, -35 ) "F", edges = [] } )
            ]
    , draggedNode = ""
    , debug = ""
    }


mouse =
    "mouse"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )

        --
        NodeViewMsg key nodeViewMsg ->
            let
                node =
                    dictGet key model.nodes

                ( node2, debug ) =
                    case nodeViewMsg of
                        -- Add a mouse node that keeps track
                        -- of the mouse's position
                        Fongf2.NodeView.AddEdge _ ->
                            ( { node
                                | edges = mouse :: node.edges
                              }
                            , "added edge"
                            )

                        -- Remove the edge when the mouse is let go
                        -- if it's in the EdgeDragging state
                        Fongf2.NodeView.LetGo ->
                            ( { node
                                | edges =
                                    case node.edges of
                                        e :: edges2 ->
                                            case node.val.mouseState of
                                                Fongf2.NodeView.EdgeDragging _ ->
                                                    edges2

                                                _ ->
                                                    e :: edges2

                                        [] ->
                                            []
                              }
                            , "removed edge"
                            )

                        _ ->
                            ( node, model.debug )

                nodes =
                    Dict.insert key
                        { node
                            | val =
                                Fongf2.NodeView.update
                                    nodeViewMsg
                                    node.val
                        }
                        model.nodes

                nodes2 =
                    case node2.val.mouseState of
                        -- Add a node that is used to connect the edges
                        Fongf2.NodeView.EdgeDragging mouseCoord ->
                            Dict.insert mouse
                                { node
                                    | val =
                                        Fongf2.NodeView.init
                                            model.width
                                            model.height
                                            mouseCoord
                                            mouse
                                            (text ""
                                                |> filled white
                                                |> makeTransparent 0
                                            )
                                    , edges = []
                                }
                                nodes

                        _ ->
                            Dict.remove mouse nodes
            in
            ( { model
                | nodes = nodes2
                , draggedNode = key
                , debug = debug
              }
            , Cmd.none
            )

        -- Add a node to model.nodes
        AddNode key ->
            ( { model
                | nodes =
                    Dict.insert key
                        { val =
                            Fongf2.NodeView.init
                                model.width
                                model.height
                                ( 0, -50 )
                                key
                                (Fongf2.NodeView.renderNode False key)
                        , edges = []
                        }
                        model.nodes
              }
            , Cmd.none
            )

        AddEdge key1 key2 ->
            ( model
            , Cmd.none
            )


renderEdges : Graph -> Shape Msg
renderEdges graph =
    let
        coord node =
            case node.val.mouseState of
                Fongf2.NodeView.NodeDragging delta ->
                    Fongf2.Util.add node.val.coord delta

                _ ->
                    node.val.coord
    in
    Dict.foldl
        (\_ node edges ->
            List.foldl
                (\key adjs ->
                    case Dict.get key graph of
                        Just adjNode ->
                            -- Draws a line from the current
                            -- node to the adj node
                            outlined (solid 2)
                                black
                                (line (coord node) (coord adjNode))
                                :: adjs

                        Nothing ->
                            adjs
                )
                []
                node.edges
                ++ edges
        )
        []
        graph
        |> group


myShapes : Model -> List (Shape Msg)
myShapes model =
    let
        -- Filter out the dragged item
        nodes =
            Dict.filter
                (\key _ -> model.draggedNode /= key)
                model.nodes

        -- Function to map NodeView.Msg to NodeViewMsg
        -- in a Shape Msg
        mapMsg key node =
            GraphicSVG.map
                (NodeViewMsg key)
                (group (Fongf2.NodeView.myShapes node.val))

        -- Remap the NodeView.Msg to NodeViewMsg
        -- and make the ship
        nodesView =
            Dict.map mapMsg nodes
    in
    [ renderEdges model.nodes
    , group <|
        Dict.values nodesView
            ++ -- Make the dragged node the last element of the list
               -- so that overlapping nodes don't cancel dragging
               (if model.draggedNode /= "" then
                    [ mapMsg model.draggedNode <|
                        dictGet model.draggedNode model.nodes
                    ]

                else
                    []
               )
    , text model.debug
        |> alignRight
        |> size 3
        |> filled black
        |> move ( 90, -60 )
    ]


view : Model -> Collage Msg
view model =
    collage 192 128 (myShapes model)


main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \_ -> ( init 600 1024, Cmd.none ) -- this requests the first random number
        , update = update
        , view = \model -> { title = "Graph Theory Visualizer", body = view model }
        , subscriptions = \_ -> Sub.none
        }
