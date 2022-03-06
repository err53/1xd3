module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as Browser
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import GraphicSVG.Widget as Widget
import Task
import Time
import Dict exposing (..)


type alias Model =
    { time : Float
    , width : Int
    , height : Int
    , widgetModel : Widget.Model 
    , state : State
    , sidebarState : SidebarState
    }

type State
    = Animating     -- make up your own states
    | NotAnimating

type SidebarState
    = Edit
    | Run

type Msg
    = WindowResize Int Int -- update the size of the window
    | Tick Float

type alias Graph =
  Dict String Node

-- contains the following info:
-- label, (x, y coords), [connected nodes]
type alias Node = 
  { coordinates : (Int, Int)
  , connections : List String
  }

initW = Widget.init 300 100 "gsvgTop"

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
            ({ model | width = width, height = height }
            , Cmd.none)
        Tick t ->
            ({ model | time = t }, Cmd.none)

view : Model -> Browser.Document Msg
view model =
    if model.width < 550 then
        phoneView model 
    else
        tabletView model 

phoneView : Model -> Browser.Document Msg
phoneView model =
        { title = "My Elm UI + Widget App"
        , body =    [E.layout []
                        (E.text "Hello phone!")
                    ]
        }

tabletView : Model -> Browser.Document Msg
tabletView model =
        { title = "My Elm UI + Widget App"
        , body =    [E.layout [
                        E.height E.fill
                      ]
                        (E.row [E.height E.fill] [
                          (E.column [E.width <| E.px (model.width // 5) 
                            , E.height E.fill
                            , Background.color <| E.rgb255 240 240 240
                            , E.padding 10
                            , Font.size 14
                            ] 
                            [ E.paragraph 
                              [ E.paddingEach {bottom = 20, left = 0, right = 0, top = 0}] 
                              [E.text "Drag Me for a ..."]
                            , E.paragraph [E.centerX] [E.text "Node"]
                            ]
                            )
                          , (E.text "World")])
                    ]
        }

renderNode id node =
  [id, node.coordinates]

main : Program () Model Msg
main =
    Browser.document
        { init = \ flags -> (initialModel, Task.perform ( \ vp -> WindowResize (round vp.viewport.width) (round vp.viewport.height)) Dom.getViewport)
        , view = view
        , update = update
        , subscriptions = \ model -> case model.state of 
                                        NotAnimating -> Browser.onResize WindowResize
                                        Animating ->
                                            Sub.batch 
                                                [ Browser.onResize WindowResize
                                                , Browser.onAnimationFrame (Time.posixToMillis 
                                                                            >> toFloat 
                                                                            >> (\ t -> 0.001 * t ) 
                                                                            >> Tick)
                                                ]
        }

