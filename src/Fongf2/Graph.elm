
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
    }


type State
    = Animating -- make up your own states
    | NotAnimating


type Msg
    = Tick Float GetKeyState
    | DraggableItemMsg String Fongf2.DraggableItem.Msg
    | AddNode String


init : Float -> Float -> Model
init width height =
    { time = 0
    , width = width
    , height = height
    , nodes =
        Dict.singleton "testing"
            { val = Fongf2.DraggableItem.init width height 
                <| renderNode "testing"
            , edges = []
            }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )

        -- Update the node named key
        DraggableItemMsg key draggableItemMsg ->
            ( { model
                | nodes =
                    Dict.insert key
                        { val =
                            Fongf2.DraggableItem.update
                                draggableItemMsg 
                                (dictGet key model.nodes).val
                        , edges = []
                        }
                        model.nodes
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
                                (renderNode key |> move ( 0, -50 ))
                        , edges = []
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


myShapes : Model -> List (Shape Msg)
-- myShapes model =
--     [ GraphicSVG.map Fongf2.DraggableItem.sg (group <| DraggableItem.myShapes model.draggableItem) ]
myShapes model =
    let
        -- Remap the DraggableItem.Msg to DraggableItemMsg
        -- and make the ship
        nodes =
            Dict.map
                (\key node -> 
                    GraphicSVG.map 
                        (DraggableItemMsg key) 
                        (group (Fongf2.DraggableItem.myShapes node.val)))
                model.nodes
    in
    [ group <| Dict.values nodes
    ]


dictGet : String -> Graph -> Node
dictGet key graph =
    let
        val =
            Dict.get key graph
    in
    case val of
        Just a ->
            a

        Nothing ->
            { val = Fongf2.DraggableItem.init 600 1024 (renderNode "error")
            , edges = []
            }


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
