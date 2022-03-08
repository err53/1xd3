module Huanj168.WinScreen exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)



-- Keyboard control for two players
-- two players are moving around, call them L and R, so we need to keep
-- track of both where they are, and where they are headed (their velocity)


init =
    { time = 0, pos = ( 50, 50 ), vel = ( 0, 0 ), angle = 0 }



-- the definition of velocity is the change in the position in a unit of time
-- the definition of acceleration is the change in the velocity in a unit of time
-- Wow!  Those sound very similar.
-- We only need function to make the change needed over one unity of time.
-- We will use the time between Ticks as the unit of time, although
-- this is not a great unit of time, since Ticks come more slowly when
-- your computer is busy, or just gets slow.


timeStep ( changeX, changeY ) ( x, y ) =
    ( x + s * changeX, y + s * changeY )



-- To make the game playable, we use a scaling factor, s.


s =
    0.1



-- You aren't supposed to know about this defintion of velocity until grade 11,
-- so don't tell anyone we told you.
-- we don't need to use any notifyTap or notifyTapAt in this type of game


myShapes model =
    [ drawShip (rgb 0 0 255) model.pos model.angle
    , Maybe.withDefault (target model) (win model)
    ]


targetPosition =
    ( -40, -30 )


target model =
    circle 5
        |> filled yellow
        |> makeTransparent (abs (sin (model.time * 2)) / 2 + 0.25)
        |> move targetPosition


win model =
    if sqrt ((Tuple.first model.pos - Tuple.first targetPosition) ^ 2 + (Tuple.second model.pos - Tuple.second targetPosition) ^ 2) <= 5 then
        Just
            (text "You win!"
                |> centered
                |> customFont "Comic Sans MS"
                |> filled black
            )

    else
        Nothing



-- nor do we need any new messages


type Msg
    = Tick Float GetKeyState



-- since we will have two ships which look alike, we will use will make a
-- function to draw them, which takes the colour as an input
-- Do you know why this ship points in the direction of motion?


drawShip clr pos angle =
    rocket clr pos
        |> scale 0.1
        |> move ( 0, -2 )
        |> rotate (-pi / 2 - angle)
        |> move pos


rocket colour animate =
    group
        [ curve ( 20, 0 ) [ Pull ( 16, 20 ) ( 0, 28 ), Pull ( -16, 20 ) ( -20, 0 ) ]
            |> filled colour
            |> scaleX 0.5
            |> move ( 0, 17 )
        , rect 19 28
            |> filled colour
            |> move ( 0, 10 )
        , polygon [ ( 0, 0 ), ( 21, -16 ), ( 14, 17 ), ( -3, 23 ) ]
            |> filled white
            |> scale 0.6
            |> move ( 5, -10 )
        , polygon [ ( 0, 0 ), ( 21, -16 ), ( 14, 17 ), ( -3, 23 ) ]
            |> filled white
            |> scale 0.6
            |> scaleX -1
            |> move ( -5, -10 )
        , isosceles 5 10
            |> filled white
            |> move ( 0, -10 )
        , circle 9
            |> filled white
            |> move ( 0, 18 )
        , circle 7
            |> filled black
            |> move ( 0, 18 )
        , circle 2
            |> filled white
            |> scaleX 0.5
            |> rotate (degrees 45)
            |> move ( 5, 21 )
        ]



-- the update function records the time (in case we want animations)
-- and uses the timeStep function to change the two positions and two
-- velocities according to their definitions


type alias Model =
    { time : Float
    , pos : Float
    , vel : Float
    , angle : Float
    }


update msg model =
    case msg of
        Tick t ( _, ( delta, thrust ), _ ) ->
            { model
                | time = t
                , pos = timeStep model.vel model.pos
                , vel =
                    let
                        positiveThrust =
                            if thrust < 0 then
                                0

                            else
                                thrust

                        accel =
                            ( positiveThrust * cos -model.angle
                            , positiveThrust * sin -model.angle
                            )
                    in
                    timeStep accel model.vel
                , angle = 0.1 * delta + model.angle
            }


main =
    gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }


view model =
    collage 192 128 (myShapes model)
