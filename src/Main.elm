module Main exposing (..)

import Dict as Dict
import Fongf2.DraggableItem as DraggableItem
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import GraphicSVG.Widget as Widget
import Task
import Time
import Array exposing (..)


type alias Model =
    { time : Float
    , width : Int
    , height : Int
    , widgetModel : Widget.Model
    , state : State
    , sidebarState : SidebarState
    , nodes : Array DraggableItem.Model
    }


type State
    = Animating -- make up your own states
    | NotAnimating


type SidebarState
    = Edit
    | Run


type Msg
    = Tick Float GetKeyState
    | DraggableItemMsg Int DraggableItem.Msg


type alias Graph =
    Dict.Dict String Node



-- contains the following info:
-- label, (x, y coords), [connected nodes]


type alias Node =
    { coordinates : ( Float, Float )
    , connections : List String
    }


initW =
    Widget.init 300 100 "gsvgTop"


deeznuts : String -> Shape DraggableItem.Msg
deeznuts txt =
    [ renderNode txt
        { coordinates = ( 0.0, 0.0 ), connections = [] }
    ]
        |> group


initialModel : Model
initialModel =
    { time = 0
    , width = 600
    , height = 1024
    , widgetModel = Tuple.first initW
    , state = NotAnimating
    , sidebarState = Edit
    , nodes = Array.fromList
        [ DraggableItem.init 600 1024 (deeznuts "test")
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )

        DraggableItemMsg idx draggableItemMsg ->
            ( { model
                | nodes = Array.set idx 
                    (DraggableItem.update draggableItemMsg (arrayGet idx model.nodes))
                    model.nodes
              }
            , Cmd.none
            )


myShapes : Model -> List (Shape Msg)
-- myShapes model =
--     [ GraphicSVG.map DraggableItemMsg (group <| DraggableItem.myShapes model.draggableItem) ]
myShapes model =
    let
        nodes =
            Array.indexedMap
                ( \idx item -> GraphicSVG.map (DraggableItemMsg idx) (group (DraggableItem.myShapes item)) )
                model.nodes
    in
    [ sidebar model
    , group <| Array.toList nodes
    ]


sidebar model =
    [ rect 40 128
        |> filled gray
        -- TODO: change the background color
        |> move ( -(192 / 2) + 20, 0 )
    , text "Drag me for a..."
        |> centered
        |> size 5
        |> filled black
        |> move ( -(192 / 2) + 20, 50 )
    , [ oval 20 10
        |> filled lightBlue
        |> move ( 0, 1 )
    , text "Node"
        |> centered
        |> size 5
        |> filled black
      ]
        |> group
        |> move ( -(192 / 2) + 20, 35 )
    ]
        |> group


arrayGet : Int -> Array DraggableItem.Model -> DraggableItem.Model
arrayGet idx arr =
    let
        val = Array.get idx arr
    in
    case val of
        Just a ->
            a
        Nothing ->
            DraggableItem.init 600 1024 (deeznuts "error")

view : Model -> Collage Msg
view model =
    collage 192 128 (myShapes model)


renderNode : String -> Node -> Shape DraggableItem.Msg
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
        { init = \_ -> ( initialModel, Cmd.none ) -- this requests the first random number
        , update = update
        , view = \model -> { title = "Graph Theory Visualizer", body = view model }
        , subscriptions = \_ -> Sub.none
        }