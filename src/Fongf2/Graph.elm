
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

type alias Model =
    { time : Float
    , width : Float
    , height : Float
    , nodes : Array Fongf2.DraggableItem.Model
    }


type State
    = Animating -- make up your own states
    | NotAnimating


type Msg
    = Tick Float GetKeyState
    | DraggableItemMsg Int Fongf2.DraggableItem.Msg
    | AddNode


type alias Graph =
    Dict.Dict String Node



-- contains the following info:
-- label, (x, y coords), [connected nodes]


type alias Node =
    { coordinates : ( Float, Float )
    , connections : List String
    }


deeznuts : String -> Shape Fongf2.DraggableItem.Msg
deeznuts txt =
    [ renderNode txt
        { coordinates = ( 0.0, 0.0 ), connections = [] }
    ]
        |> group


init : Float -> Float -> Model
init width height =
    { time = 0
    , width = width
    , height = height
    , nodes =
        Array.fromList
            [ Fongf2.DraggableItem.init width height (deeznuts "test")
            ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )

        DraggableItemMsg idx draggableItemMsg ->
            ( { model
                | nodes =
                    Array.set idx
                        (Fongf2.DraggableItem.update draggableItemMsg (arrayGet idx model.nodes))
                        model.nodes
              }
            , Cmd.none
            )

        AddNode ->
            ( { model
                | nodes =
                    Array.push
                        (Fongf2.DraggableItem.init
                            model.width model.height
                            (deeznuts "A" |> move ( 0, -50 ))
                        )
                        model.nodes
              }
            , Cmd.none
            )


myShapes : Model -> List (Shape Msg)
-- myShapes model =
--     [ GraphicSVG.map Fongf2.DraggableItem.sg (group <| DraggableItem.myShapes model.draggableItem) ]
myShapes model =
    let
        nodes =
            Array.indexedMap
                (\idx item -> 
                    GraphicSVG.map (DraggableItemMsg idx) (group (Fongf2.DraggableItem.myShapes item)))
                model.nodes
    in
    [ group <| Array.toList nodes
    ]


arrayGet : Int -> Array Fongf2.DraggableItem.Model -> Fongf2.DraggableItem.Model
arrayGet idx arr =
    let
        val =
            Array.get idx arr
    in
    case val of
        Just a ->
            a

        Nothing ->
            Fongf2.DraggableItem.init 600 1024 (deeznuts "error")


view : Model -> Collage Msg
view model =
    collage 192 128 (myShapes model)


renderNode : String -> Node -> Shape Fongf2.DraggableItem.Msg
renderNode key node =
    [ oval 20 10
        |> filled gray
        |> move ( 0, 1 )
    , text key
        |> centered
        |> size 4
        |> filled black
    ]
        |> group
        |> move node.coordinates


main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \_ -> ( init 600 1024, Cmd.none ) -- this requests the first random number
        , update = update
        , view = \model -> { title = "Graph Theory Visualizer", body = view model }
        , subscriptions = \_ -> Sub.none
        }
