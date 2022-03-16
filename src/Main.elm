module Main exposing (..)

import Array exposing (..)
import Browser
import Dict as Dict
import Fongf2.Graph
import Simiones.DownloadTxt
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import GraphicSVG.Widget as Widget
import Task
import Time
import Html
import Html.Attributes as HA

image : Float -> Float -> String-> Shape userMsg
image w h url = html w h (Html.img [HA.height (floor h), HA.width (floor w), HA.src url] [])

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
    | Download String


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
        Download text ->  ( model, Simiones.DownloadTxt.save text )

myShapes : Model -> List (Shape Msg)
myShapes model =
    let
        graph =
            Fongf2.Graph.myShapes model.graphModel
    in
    [ sidebar model
    , downloadButton
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

downloadButton : Shape Msg
downloadButton = 
    group
    [ roundedRect 36 20 2
        |> filled lightGray
    , text "Download as .csv"
        |> centered
        |> size 4
        |> filled black
        |> move (0,2)
    , image 20 20 "https://cdn-icons-png.flaticon.com/512/0/532.png"
        |> move (-20/2,20/2)
        |> scale 0.4
        |> move (0,-4)
    ]
    |> move ( -(192 / 2) + 20, -50 )
    |> notifyTap (Download "hello")

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
