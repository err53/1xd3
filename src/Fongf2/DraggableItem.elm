module Fongf2.DraggableItem exposing (..)

import Dict exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
type alias Coord = (Float, Float)


sub : Coord -> Coord -> Coord
sub (x, y) (u, v) = (x - u, y - v)


add : Coord -> Coord -> Coord
add (x, y) (u, v) = (x + u, y + v)


type Msg 
  = Tick Float GetKeyState
  | NewCoord Coord
  | LetGo
  

type MouseState
  = Waiting
  | Dragging Coord
  

type alias Model = 
  { time : Float
  , width : Float
  , height : Float
  , mouseState : MouseState
  , coord : Coord
  , shape : Shape Msg
  , debug : String
  }
  

update : Msg -> Model -> Model
update msg model = 
  case msg of
    Tick t _ ->
      { model | time = t }
      
    NewCoord coord ->
      { model
      | coord = coord
      , mouseState = 
        case model.mouseState of
          Waiting ->
            Dragging (sub model.coord coord)
          _ ->
            model.mouseState
    --   , 
    --   debug = 
    --     let
    --         (x, y) = coord
    --     in
    --         String.fromFloat x ++ ", " ++ String.fromFloat y
      }
      
    LetGo ->
      case model.mouseState of
        Dragging delta ->
          { model
          | coord = add model.coord delta
          , mouseState = Waiting
          }
        _ ->
          model
    

-- Pass in the shape that you want to make draggable
init : Float -> Float -> Shape Msg ->  Model
init width height shape = 
  { time = 0
  , width = width
  , height = height
  , mouseState = Waiting
  , coord = (0, 0)
  , shape = shape
  , debug = ""
  }


myShapes : Model -> List (Shape Msg)
myShapes model =
  let
    (coord, notification) =
      case model.mouseState of
        Waiting ->
          ( model.coord
          , notifyMouseDownAt NewCoord
          )
        Dragging delta ->
          ( add model.coord delta
          , identity
          )
          
    visible =
      case model.mouseState of
        Dragging _ -> (0, 0)
        _ -> (10000, 10000)
  in
  [ model.shape
    |> move coord
    |> notification
  , text model.debug
    |> filled black
    |> move (0, 20)
  , rect model.width model.height 
    |> filled white
    |> makeTransparent 0
    |> move visible
    -- TODO releasing the mouse while moving does not release the circle
    |> notifyMouseMoveAt NewCoord
    |> notifyMouseUp LetGo
    |> notifyLeave LetGo
  ]
  
  
view : Model -> Collage Msg
view model =
    collage model.width model.height <|
        List.concat <|
            [ myShapes model
            , [ text model.debug 
                |> GraphicSVG.size 4 
                |> alignRight 
                |> filled black 
                |> move ( 90, -62 )
              ]
            ]
            
  
main = gameApp Tick { model = init 192 128 (circle 20 |> filled red), view = view, update = update, title = "Game Slot" }