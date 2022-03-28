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



-- graph and edges of graph
-- Displays the graph on the screen


type alias Coord =
    ( Float, Float )


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
    , graph : Graph
    , selectedNode : String
    , mouseCoord : Coord
    , isDragging : Bool
    , debug : String
    }


type Msg
    = Tick Float GetKeyState
    | NodeViewMsg String Fongf2.NodeView.Msg
    | AddNode String
    | ConnectEdge String



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
    , graph =
        --Dict.empty
        Dict.fromList
            [ ( "A", { val = node ( 50, 50 ) "A", edges = [ "B", "C" ] } )
            , ( "B", { val = node ( 50, 0 ) "B", edges = [ "A" ] } )
            , ( "C", { val = node ( -25, 0 ) "C", edges = [ "D" ] } )
            , ( "D", { val = node ( -25, -25 ) "D", edges = [ "A", "C", "F", "E" ] } )
            , ( "E", { val = node ( 0, 50 ) "E", edges = [ "F", "E", "B" ] } )
            , ( "F", { val = node ( 0, -35 ) "F", edges = [] } )
            ]
    , selectedNode = ""
    , mouseCoord = ( 0, 0 )
    , isDragging = False
    , debug = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )

        NodeViewMsg key nodeViewMsg ->
            let
                node =
                    dictGet key model.graph

                ( mouseCoord, isDragging, debug ) =
                    case node.val.mouseState of
                        -- If an edge is added from a node, then
                        -- change the mouse's coordinates
                        Fongf2.NodeView.EdgeDragging _ ->
                            case nodeViewMsg of
                                Fongf2.NodeView.NewNodeCoord coord ->
                                    ( coord, True, "edge dragging" )

                                _ ->
                                    ( node.val.coord, False, "edge waiting" )

                        _ ->
                            ( node.val.coord, False, "edge waiting 2" )

                ( x, y ) =
                    mouseCoord
            in
            ( { model
                | graph =
                    if isDragging then
                        model.graph

                    else
                        Dict.insert key
                            { node
                                | val =
                                    Fongf2.NodeView.update
                                        nodeViewMsg
                                        node.val
                            }
                            model.graph
                , selectedNode = key
                , mouseCoord = mouseCoord
                , isDragging = isDragging
                , debug = model.selectedNode
              }
            , Cmd.none
            )

        -- Add a node to model.graph
        AddNode key ->
            ( { model
                | graph =
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
                        model.graph
              }
            , Cmd.none
            )

        ConnectEdge key ->
            let
                node =
                    dictGet key model.graph

                selectedNode =
                    dictGet model.selectedNode model.graph

                node2 =
                    Fongf2.NodeView.update
                        Fongf2.NodeView.LetGo
                        node.val
            in
            ( { model
                | graph =
                    Dict.insert model.selectedNode
                        { node
                            | val =
                                Fongf2.NodeView.update
                                    Fongf2.NodeView.LetGo
                                    selectedNode.val
                        }
                        model.graph
                , mouseCoord = selectedNode.val.coord
                , isDragging = False
                , debug =
                    List.foldl
                        (\( key1, node3 ) edges ->
                            List.foldl
                                (\key2 adjs -> key2 ++ " ")
                                ""
                                node3.edges
                                ++ "     "
                                ++ edges
                        )
                        ""
                        (Dict.toList model.graph)
              }
            , Cmd.none
            )


renderEdges : Model -> Shape Msg
renderEdges model =
    let
        coord node =
            case node.val.mouseState of
                Fongf2.NodeView.NodeDragging delta ->
                    Fongf2.Util.add node.val.coord delta

                _ ->
                    node.val.coord

        makeLine coord1 coord2 =
            outlined (solid 2) black (line coord1 coord2)

        draggedEdge =
            (if model.isDragging then
                [ makeLine
                    (dictGet model.selectedNode model.graph).val.coord
                    model.mouseCoord
                ]

             else
                []
            )
                |> group
    in
    Dict.foldl
        (\key1 node edges ->
            List.foldl
                (\key adjs ->
                    case Dict.get key model.graph of
                        Just adjNode ->
                            let
                                ( ( x1, y1 ), ( x2, y2 ) ) =
                                    ( coord node, coord adjNode )

                                avg a b =
                                    (a + b) / 2
                            in
                            -- Draws a line from the current
                            -- node to the adj node
                            ([ makeLine (coord node) (coord adjNode)
                             , text (key1 ++ key)
                                |> size 4
                                |> filled red
                                |> move ( avg x1 x2, avg y1 y2 )
                             ]
                                |> group
                            )
                                :: adjs

                        Nothing ->
                            adjs
                )
                []
                node.edges
                ++ edges
        )
        [ draggedEdge ]
        model.graph
        |> group


renderGraph : Model -> Shape Msg
renderGraph model =
    let
        -- Filter out the dragged item
        graph =
            Dict.filter
                (\key _ -> model.selectedNode /= key)
                model.graph

        -- Function to map NodeView.Msg to NodeViewMsg
        -- in a Shape Msg
        mapMsg key node =
            GraphicSVG.map
                (NodeViewMsg key)
                (group (Fongf2.NodeView.myShapes node.val))

        -- Remap the NodeView.Msg to NodeViewMsg
        -- and make the ship
        graphView =
            Dict.map mapMsg graph

        dropboxes =
            List.map
                (\( key, node ) ->
                    let
                        coord =
                            case node.val.mouseState of
                                Fongf2.NodeView.NodeDragging delta ->
                                    Fongf2.Util.add node.val.coord delta

                                _ ->
                                    node.val.coord
                    in
                    oval 20 10
                        |> filled (rgba 255 0 0 0.5)
                        |> move coord
                        |> notifyMouseUp (ConnectEdge key)
                        |> notifyMouseMoveAt (NodeViewMsg model.selectedNode << Fongf2.NodeView.NewNodeCoord)
                )
                (Dict.toList model.graph)
                |> group
                |> scale
                    (if model.isDragging then
                        1

                     else
                        0
                    )
    in
    [ group <|
        Dict.values graphView
            ++ -- Make the dragged node the last element of the list
               -- so that overlapping graph don't cancel dragging
               (if model.selectedNode /= "" then
                    [ mapMsg model.selectedNode <|
                        dictGet model.selectedNode model.graph
                    ]

                else
                    []
               )
    , dropboxes
    ]
        |> group


myShapes : Model -> List (Shape Msg)
myShapes model =
    [ renderEdges model
    , renderGraph model
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
