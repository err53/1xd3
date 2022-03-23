module Main exposing (..)

import Array exposing (..)
import Browser
import Dict as Dict
import Fongf2.Graph
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import GraphicSVG.Widget as Widget
import Task
import Time


type alias Model =
    { time : Float
    , width : Float
    , height : Float
    , widgetModel : Widget.Model
    , state : State
    , sidebarState : SidebarState
    , graphModel : Fongf2.Graph.Model
    }


type State
    = Animating -- make up your own states
    | NotAnimating


type SidebarState
    = Edit
    | Run


type Msg
    = Tick Float GetKeyState
    | GraphMsg Fongf2.Graph.Msg


initW =
    Widget.init 300 100 "gsvgTop"


initialModel : Model
initialModel =
    { time = 0
    , width = 600
    , height = 1024
    , widgetModel = Tuple.first initW
    , state = NotAnimating
    , sidebarState = Edit
    , graphModel = Fongf2.Graph.init 600 1024
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )

        -- Update the graph being displayed
        GraphMsg graphMsg ->
            let
                ( newGraphModel, newGraphMsg ) =
                    Fongf2.Graph.update graphMsg model.graphModel
            in
            ( { model
                | graphModel = newGraphModel
              }
            , Cmd.map GraphMsg newGraphMsg
            )


myShapes : Model -> List (Shape Msg)
myShapes model =
    let
        graph =
            Fongf2.Graph.myShapes model.graphModel
    in
    [ sidebar model
    , GraphicSVG.map GraphMsg (group graph)
    ]


sidebar : Model -> Shape Msg
sidebar model =
    let
        graphMsg =
            GraphMsg <|
                Fongf2.Graph.AddNode <|
                    String.fromInt <|
                        Dict.size model.graphModel.nodes
    in
    [ rect 40 128
        |> filled gray
        -- TODO: change the background color
        |> move ( -(192 / 2) + 20, 0 )
    , text "Drag me for a..."
        |> centered
        |> size 5
        |> filled black
        |> move ( -(192 / 2) + 20, 50 )

    -- Button to add a node into the graph
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
        -- TODO UPDATE THE NAMING OF THE NODES
        |> notifyTap graphMsg
    ]
        |> group


view : Model -> Collage Msg
view model =
    collage 192 128 (myShapes model)


main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \_ -> ( initialModel, Cmd.none ) -- this requests the first random number
        , update = update
        , view = \model -> { title = "Graph Theory Visualizer", body = view model }
        , subscriptions = \_ -> Sub.none
        }
