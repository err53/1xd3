<<<<<<< HEAD
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
=======
module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as Browser
import Dict as Dict
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Fongf2.DraggableItem
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
    }


type State
    = Animating -- make up your own states
    | NotAnimating


type SidebarState
    = Edit
    | Run


type Msg
    = WindowResize Int Int -- update the size of the window
    | Tick Float


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


initialModel : Model
initialModel =
    { time = 0
    , width = 600
    , height = 1024
    , widgetModel = Tuple.first initW
    , state = NotAnimating
    , sidebarState = Edit
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize width height ->
            ( { model | width = width, height = height }
            , Cmd.none
            )

        Tick t ->
            ( { model | time = t }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    if model.width < 550 then
        phoneView model

    else
        tabletView model


phoneView : Model -> Browser.Document Msg
phoneView model =
    { title = "My Elm UI + Widget App"
    , body =
        [ E.layout []
            (E.text "Hello phone!")
        ]
    }


tabletView : Model -> Browser.Document Msg
tabletView model =
    { title = "My Elm UI + Widget App"
    , body =
        [ E.layout
            [ E.height E.fill
            ]
            (E.row [ E.height E.fill ]
                [ E.column
                    [ E.width <| E.px 250
                    , E.height E.fill
                    , Background.color <| E.rgb255 240 240 240
                    , E.padding 10
                    , Font.size 18
                    , Font.center
                    ]
                    [ E.paragraph
                        [ E.paddingEach { bottom = 20, left = 0, right = 0, top = 0 } ]
                        [ E.text "Drag Me for a ..." ]
                    , E.paragraph [ E.centerX ] [ E.text "Node" ]
                    ]
                , E.el
                    [ E.width <| E.px (model.width - 250) -- this is incredibly janky
                    ]
                    (E.html
                        (Widget.view model.widgetModel
                            [ renderNode "testing" { coordinates = ( 0.0, 0.0 ), connections = [] }
                            ]
                        )
                    )
                ]
            )
        ]
    }


renderNode : String -> Node -> Shape Msg
renderNode key node =
    [ oval 100 50
        |> filled gray
        |> move ( 0, 4 )
    , text key
        |> centered
        |> size 16
        |> filled black
    ]
        |> group
        |> move node.coordinates


main : Program () Model Msg
main =
    Browser.document
        { init =
            \flags ->
                ( initialModel
                , Task.perform
                    (\vp ->
                        WindowResize
                            (round vp.viewport.width)
                            (round vp.viewport.height)
                    )
                    Dom.getViewport
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                case model.state of
                    NotAnimating ->
                        Browser.onResize WindowResize

                    Animating ->
                        Sub.batch
                            [ Browser.onResize WindowResize
                            , Browser.onAnimationFrame
                                (Time.posixToMillis
                                    >> toFloat
                                    >> (\t -> 0.001 * t)
                                    >> Tick
                                )
                            ]
        }
>>>>>>> 3af479a36355f81d34aa4aad464a7326ef2596e5
