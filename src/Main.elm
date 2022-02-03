module Main exposing (..)

import Fongf2.RocketGame
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Huanj168.WinScreen
import Nagii.RocketMoving


myShapes model =
    case model.state of
        Start ->
            [ Nagii.RocketMoving.myShapes model
                |> group
            , group
                [ roundedRect 40 20 5
                    |> filled green
                , text "ToAstroid"
                    |> centered
                    |> size 8
                    |> filled black
                    |> move ( 0, -3 )
                ]
                |> notifyTap ToAstroid
            ]

        AstroidScreen ->
            [ if model.keyV == -1 then
                fire
                    |> move ( 0, 12 )
                    |> move model.posL

              else
                circle 15
                    |> filled (rgba 0 0 0 0)
            , if model.keyV == 1 then
                fire
                    |> scale -1
                    |> move ( 0, -12 )
                    |> move model.posL

              else
                circle 15
                    |> filled (rgba 0 0 0 0)
            , if model.keyH == -1 then
                fire
                    |> rotate (degrees 90)
                    |> scale -1
                    |> move ( 12, 0 )
                    |> move model.posL

              else
                circle 15
                    |> filled (rgba 0 0 0 0)
            , if model.keyH == 1 then
                fire
                    |> rotate (degrees 270)
                    |> scale -1
                    |> move ( -12, 0 )
                    |> move model.posL

              else
                circle 15
                    |> filled (rgba 0 0 0 0)
            , drawAstroid (rgb 255 0 0) model.posL model.velL
            , group
                [ roundedRect 40 20 5
                    |> filled green
                , text "ToRocket"
                    |> centered
                    |> size 8
                    |> filled black
                    |> move ( 0, -3 )
                ]
                |> move ( 0, -50 )
                |> notifyTap ToRocket
            ]

        RocketScreen ->
            [ group <| Fongf2.RocketGame.myShapes model
            , group
                [ roundedRect 40 20 5
                    |> filled green
                , text "ToWin"
                    |> centered
                    |> size 8
                    |> filled black
                    |> move ( 0, -3 )
                ]
                |> move ( -25, -25 )
                |> notifyTap ToWin
            , group
                [ roundedRect 40 20 5
                    |> filled green
                , text "ToAstroid"
                    |> centered
                    |> size 8
                    |> filled black
                    |> move ( 0, -3 )
                ]
                |> move ( 25, -25 )
                |> notifyTap ToAstroid
            ]

        WinScreen ->
            [ text "WinScreen"
                |> centered
                |> filled black
            , group
                [ roundedRect 40 20 5
                    |> filled green
                , text "ToRocket"
                    |> centered
                    |> size 8
                    |> filled black
                    |> move ( 0, -3 )
                ]
                |> move ( 0, -25 )
                |> notifyTap ToRocket
            ]



-- Huanj168.WinScreen.myShapes model


type Msg
    = Tick Float GetKeyState
    | ToRocket
    | ToWin
    | ToAstroid


type State
    = Start
    | AstroidScreen
    | RocketScreen
    | WinScreen


update msg model =
    case msg of
        Tick t ( _, accelR, accelL ) ->
            case model.state of
                AstroidScreen ->
                    { model
                        | time = t
                        , posL = timeStep model.velL model.posL
                        , velL = timeStep accelR model.velL
                        , keyV = Tuple.second accelR
                        , keyH = Tuple.first accelR
                    }

                otherwise ->
                    { model
                        | time = t
                        , posR = timeStep model.velR model.posR
                        , velR =
                            let
                                positiveThrust =
                                    if Tuple.second accelR < 0 then
                                        0

                                    else
                                        Tuple.second accelR

                                accel =
                                    ( positiveThrust * cos -model.angleR
                                    , positiveThrust * sin -model.angleR
                                    )
                            in
                            timeStep accel model.velR
                        , angleR = 0.1 * Tuple.first accelR + model.angleR
                    }

        ToRocket ->
            case model.state of
                AstroidScreen ->
                    { model | state = RocketScreen }

                WinScreen ->
                    { model | state = RocketScreen }

                otherwise ->
                    model

        ToWin ->
            case model.state of
                RocketScreen ->
                    { model | state = WinScreen }

                otherwise ->
                    model

        ToAstroid ->
            case model.state of
                Start ->
                    { model | state = AstroidScreen }

                RocketScreen ->
                    { model | state = AstroidScreen }

                otherwise ->
                    model


type alias Model =
    { time : Float
    , state : State
    , keyH : Int
    , keyV : Int
    , astroidScreenModel : Fongf2.RocketGame.Model
    , winScreenModel : Huanj168.WinScreen.Model
    }


type alias Point =
    ( Float, Float )


init =
    { time = 0
    , state = Start
    , posL = ( 0, 0 )
    , velL = ( 0, 0 )
    , velR = ( 0, 0 )
    , keyH = 0
    , keyV = 0
    , angleR = 0
    }


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
