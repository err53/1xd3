module Fongf2.Graph exposing (..)

import Array exposing (..)
import Dict as Dict
import Fongf2.NodeView
import Fongf2.Util
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import GraphicSVG.Widget as Widget
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
            , ( "F", { val = node ( 0, -50 ) "F", edges = [] } )
            ]
    , draggedNode = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )

        -- Update the node named key
        NodeViewMsg key nodeViewMsg ->
            let
                node =
                    dictGet key model.nodes
            in
            ( { model
                | nodes =
                    Dict.insert key
                        { node
                            | val =
                                Fongf2.NodeView.update
                                    nodeViewMsg
                                    node.val
                        }
                        model.nodes
                , draggedNode = key
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
    Dict.foldl
        (\_ node edges ->
            List.foldl
                (\key adjs ->
                    case Dict.get key graph of
                        Just adjNode ->
                            let
                                coord =
                                    case node.val.mouseState of
                                        Fongf2.NodeView.NodeDragging delta ->
                                            Fongf2.Util.add node.val.coord delta

                                        _ ->
                                            node.val.coord

                                adjCoord =
                                    case adjNode.val.mouseState of
                                        Fongf2.NodeView.NodeDragging delta ->
                                            Fongf2.Util.add adjNode.val.coord delta

                                        _ ->
                                            adjNode.val.coord
                            in
                            -- Draws a line from the current
                            -- node to the adj node
                            outlined (solid 2)
                                black
                                (line coord adjCoord)
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
