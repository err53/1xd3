module Main exposing (..)

import Dict as Dict
import Fongf2.DraggableItem as DraggableItem
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import GraphicSVG.Widget as Widget
import Task
import Time


type alias Model =
    { time : Float
    , width : Int
    , height : Int
    , widgetModel : Widget.Model
    , state : State
    , sidebarState : SidebarState
    , draggableItem : DraggableItem.Model
    }


type State
    = Animating -- make up your own states
    | NotAnimating


type SidebarState
    = Edit
    | Run


type Msg
    = Tick Float GetKeyState
    | DraggableItemMsg DraggableItem.Msg


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


deeznuts =
    [ renderNode "test"
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
    , draggableItem =
        DraggableItem.init
            600
            1024
            deeznuts
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )

        DraggableItemMsg draggableItemMsg ->
            ( { model
                | draggableItem = DraggableItem.update draggableItemMsg model.draggableItem
              }
            , Cmd.none
            )


myShapes : Model -> List (Shape Msg)



-- myShapes model =
--     [ GraphicSVG.map DraggableItemMsg (group <| DraggableItem.myShapes model.draggableItem) ]


myShapes model =
    [ sidebar model
    , GraphicSVG.map DraggableItemMsg
        (group <|
            DraggableItem.myShapes
                model.draggableItem
        )
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
    , text "Node"
        |> centered
        |> size 5
        |> filled black
        |> move ( -(192 / 2) + 20, 35 )
    ]
        |> group


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
