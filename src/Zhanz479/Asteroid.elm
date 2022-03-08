module Zhanz479.Asteroid exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


timeStep ( changeX, changeY ) ( x, y ) =
    ( x + 0.1 * changeX, y + 0.1 * changeY )


drawAstroid clr pos ( vx, vy ) =
    --draws the astroid shape with the astroid pattern at given position
    group
        [ astroidShape
            |> addOutline (solid 1) black
        , astroidPat
            |> clip astroidShape
            |> addOutline (solid 1) black
        ]
        |> move pos


myShapes model =
    [ if model.keyV == -1 then
        fire
            |> move ( 0, 12 )
            |> move model.pos

      else
        circle 15
            |> filled (rgba 0 0 0 0)
    , if model.keyV == 1 then
        fire
            |> scale -1
            |> move ( 0, -12 )
            |> move model.pos

      else
        circle 15
            |> filled (rgba 0 0 0 0)
    , if model.keyH == -1 then
        fire
            |> rotate (degrees 90)
            |> scale -1
            |> move ( 12, 0 )
            |> move model.pos

      else
        circle 15
            |> filled (rgba 0 0 0 0)
    , if model.keyH == 1 then
        fire
            |> rotate (degrees 270)
            |> scale -1
            |> move ( -12, 0 )
            |> move model.pos

      else
        circle 15
            |> filled (rgba 0 0 0 0)
    , drawAstroid (rgb 255 0 0) model.pos model.vel
    ]


type Msg
    = Tick Float GetKeyState


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick t ( _, accel, _ ) ->
            { model
                | time = t
                , pos = timeStep model.vel model.pos
                , vel = timeStep accel model.vel
                , keyV = Tuple.second accel
                , keyH = Tuple.first accel
            }


type alias Model =
    { time : Float
    , pos : Point
    , vel : Point
    , keyH : Float
    , keyV : Float
    }


type alias Point =
    ( Float, Float )


init : Model
init =
    { time = 0
    , pos = ( 0, 0 )
    , vel = ( 0, 0 )
    , keyH = 0
    , keyV = 0
    }


main : GameApp Model Msg
main =
    gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }


view model =
    collage 192 128 (myShapes model)


fire =
    isosceles 5 10
        |> filled (rgb 0 0 255)


astroidShape =
    circle 12
        --circular shape of astroid
        |> filled gray


astroidPat =
    group
        --circular crators design on astroid
        [ circle 3
            |> filled (rgb 115 115 120)
            |> move ( 10, 6 )
        , circle 4
            |> filled (rgb 115 115 120)
            |> move ( -5, 2 )
        , circle 3.5
            |> filled (rgb 115 115 120)
            |> move ( 5, -9 )
        , circle 5
            |> filled (rgb 115 115 120)
            |> move ( -9, -9 )
        , circle 2
            |> filled (rgb 115 115 120)
            |> move ( -5, -6 )
        , circle 2.5
            |> filled (rgb 115 115 120)
            |> move ( -3, 5 )
        , circle 1.5
            |> filled (rgb 115 115 120)
            |> move ( 5, -5 )
        , circle 1
            |> filled (rgb 115 115 120)
            |> move ( 5, 10 )
        , circle 1.5
            |> filled (rgb 115 115 120)
            |> move ( 7, 7 )
        , circle 2
            |> filled (rgb 115 115 120)
            |> move ( 8, 3 )
        ]
