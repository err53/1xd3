module Fongf2.Graph exposing (..)

import Array exposing (..)
import Dict as Dict
import Fongf2.DraggableItem
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import GraphicSVG.Widget as Widget
import Task
import Time


-- Nodes and edges of graph
-- Displays the graph on the screen


type alias Node =
    { val : Fongf2.DraggableItem.Model 
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
    | DraggableItemMsg String Fongf2.DraggableItem.Msg
    | AddNode String


--* Get the node from the graph given a key
dictGet : String -> Graph -> Node
dictGet key graph =
    case Dict.get key graph of
        Just a ->
            a

        Nothing ->
            { val = Fongf2.DraggableItem.init 600 1024 (0, 0)
                <| renderNode "error"
            , edges = []
            }


init : Float -> Float -> Model
init width height =
    { time = 0
    , width = width
    , height = height
    , nodes = Dict.empty
    , draggedNode = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )

        -- Update the node named key
        DraggableItemMsg key draggableItemMsg ->
            let
                node = dictGet key model.nodes
            in
            ( { model
                | nodes =
                    Dict.insert key
                        { node
                        | val =
                            Fongf2.DraggableItem.update
                                draggableItemMsg 
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
                            Fongf2.DraggableItem.init
                                model.width model.height
                                (0, -50) (renderNode key)
                        , edges = [ "testing" ]
                        }
                        model.nodes
              }
            , Cmd.none
            )


renderNode : String -> Shape Fongf2.DraggableItem.Msg
renderNode txt =
    [ oval 20 10
        |> filled gray
    , text txt
        |> centered
        |> size 4
        |> filled black
    ]
        |> group
        

renderEdges : Graph -> Shape Msg
renderEdges graph =
    Dict.foldl (\_ node edges ->
        List.foldl (\key adjs -> 
            case Dict.get key graph of
                Just adjNode -> 
                    -- Draws a line from the current
                    -- node to the adj node
                    outlined (solid 2) black
                        (line node.val.coord adjNode.val.coord)
                    :: adjs
                Nothing ->
                    adjs
        ) [] node.edges ++ edges
    ) [] graph
        |> group


myShapes : Model -> List (Shape Msg)
myShapes model =
    let
        -- Filter out the dragged item
        nodes = 
            Dict.filter 
                (\key _ -> model.draggedNode/=key)
                model.nodes

        -- Function to map DraggableItem.Msg to DraggableItemMsg
        -- in a Shape Msg
        mapMsg key node =
            GraphicSVG.map
                (DraggableItemMsg key)
                (group (Fongf2.DraggableItem.myShapes node.val))

        -- Remap the DraggableItem.Msg to DraggableItemMsg
        -- and make the ship
        nodesView =
            Dict.map mapMsg nodes
    in
    [ renderEdges model.nodes
    , group 
        <| Dict.values nodesView 
        ++ 
        -- Make the dragged node the last element of the list
        -- so that overlapping nodes don't cancel dragging
        if model.draggedNode/="" then
            [ mapMsg model.draggedNode 
                <| dictGet model.draggedNode model.nodes
            ]
        else []
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
