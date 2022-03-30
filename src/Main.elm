module Main exposing (..)

import Array exposing (..)
import Browser
import Dict as Dict
import Fongf2.Graph
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import GraphicSVG.Widget as Widget
import Html
import Html.Attributes as HA
import Simiones.DownloadTxt
import Task
import Time


image : Float -> Float -> String -> Shape userMsg
image w h url =
    html w h (Html.img [ HA.height (floor h), HA.width (floor w), HA.src url ] [])


type alias Model =
    { time : Float
    , width : Float
    , height : Float
    , widgetModel : Widget.Model
    , state : State
    , sidebarState : SidebarState
    , graphModel : Fongf2.Graph.Model
    , debug : String
    }


type State
    = Animating -- make up your own states
    | NotAnimating


type SidebarState
    = Edit
    | Deleting
    | Run


type Msg
    = Tick Float GetKeyState
    | GraphMsg Fongf2.Graph.Msg
    | Download String
    | ToggleDeleteMode


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
    , debug = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t keys ->
            let
                ( graphModel, graphCmd ) =
                    Fongf2.Graph.update
                        (Fongf2.Graph.Tick
                            t
                            keys
                        )
                        model.graphModel
            in
            ( { model
                | time = t
                , graphModel = graphModel
              }
            , Cmd.none
            )

        -- Update the graph being displayed
        GraphMsg graphMsg ->
            let
                ( ( graphModel, _ ), debug ) =
                    case model.sidebarState of
                        Deleting ->
                            case graphMsg of
                                Fongf2.Graph.PressingNode key ->
                                    ( Fongf2.Graph.update
                                        (Fongf2.Graph.DeleteNode key)
                                        model.graphModel
                                    , "deleted node"
                                    )

                                Fongf2.Graph.PressingEdge key1 key2 ->
                                    ( Fongf2.Graph.update
                                        (Fongf2.Graph.DeleteEdge key1 key2)
                                        model.graphModel
                                    , "Deleted edge"
                                    )

                                _ ->
                                    ( ( model.graphModel, Cmd.none ), "nothing" )

                        _ ->
                            ( ( model.graphModel, Cmd.none ), "nothing2" )

                ( newGraphModel, newGraphMsg ) =
                    Fongf2.Graph.update graphMsg graphModel
            in
            ( { model
                | graphModel = newGraphModel
              }
            , Cmd.map GraphMsg newGraphMsg
            )

        Download text ->
            ( model, Simiones.DownloadTxt.save text )

        ToggleDeleteMode ->
            let
                ( newState, debug ) =
                    case model.sidebarState of
                        Deleting ->
                            ( Edit, "edit" )

                        _ ->
                            ( Deleting, "remove" )
            in
            ( { model
                | sidebarState = newState
              }
            , Cmd.none
            )


myShapes : Model -> List (Shape Msg)
myShapes model =
    let
        graph =
            Fongf2.Graph.myShapes model.graphModel
    in
    [ sidebar model
    , downloadButton
        |> notifyTap (Download (Simiones.DownloadTxt.adjacencyList model.graphModel.graph))
    , GraphicSVG.map GraphMsg (group graph)
    ]


sidebar : Model -> Shape Msg
sidebar model =
    let
        addNodeMsg =
            GraphMsg Fongf2.Graph.AddNode
    in
    [ rect 40 128
        |> filled lightGrey
    , text "Drag me for a..."
        |> centered
        |> size 4
        |> filled black
        |> move ( 0, 50 )

    -- Button to add a node into the graph
    , [ oval 20 10
            |> filled (rgb 180 244 239)
            |> move ( 0, 1.5 )
      , text "Node"
            |> centered
            |> size 4
            |> filled black
      ]
        |> group
        |> move ( 0, 35 )
        -- TODO UPDATE THE NAMING OF THE graph
        |> notifyMouseDown addNodeMsg
    , line ( -17, 0 ) ( 17, 0 )
        |> outlined (solid 0.5) (rgb 207 207 207)
        |> move ( 0, 26 )

    -- Button to go into delete mode
    , group
        [ roundedRect 34 20 3
            |> filled grey
            |> move ( 0, -3 )
            |> makeTransparent
                (if model.sidebarState == Deleting then
                    1

                 else
                    0
                )
        , image 15 15 "svg/eraser-solid.svg"
            |> move ( -8, 6 )
        ]
        |> move ( 0, 17 )
        |> notifyTap ToggleDeleteMode
    ]
        |> group
        |> move ( -(192 / 2) + 20, 0 )


downloadButton : Shape Msg
downloadButton =
    group
        [ roundedRect 36 20 2
            |> filled grey
        , text "Download as .csv"
            |> centered
            |> size 4
            |> filled black
            |> move ( 0, 2 )
        , image 20 20 "https://cdn-icons-png.flaticon.com/512/0/532.png"
            |> move ( -20 / 2, 20 / 2 )
            |> scale 0.4
            |> move ( 0, -4 )
        ]
        |> move ( -(192 / 2) + 20, -50 )


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
