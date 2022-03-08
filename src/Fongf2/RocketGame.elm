module Fongf2.RocketGame exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Simiones.RocketAdventure


s =
    0.05



-- we don't need to use any notifyTap or notifyTapAt in this type of game


myShapes model =
    [ Simiones.RocketAdventure.spaceBackground model.time
    , drawShip (rgb 255 0 0) model
    ]



-- since we will have two ships which look alike, we will use will make a
-- function to draw them, which takes the colour as an input
-- Do you know why this ship points in the direction of motion?


drawShip colour model =
    Simiones.RocketAdventure.rocket colour model.time
        |> scale 0.6
        |> rotate (degrees 90)
        |> rotate (pi - model.angle)
        |> move model.pos


type Msg
    = Tick Float GetKeyState


type alias Point =
    ( Float, Float )


type alias Model =
    { time : Float
    , pos : Point
    , vel : Point
    , angle : Float
    }


init : Model
init =
    { time = 0
    , pos = ( 0, 0 )
    , vel = ( 0, 0 )
    , angle = -pi / 2
    }



-- We only need function to make the change needed over one unity of time.
-- We will use the time between Ticks as the unit of time, although
-- this is not a great unit of time, since Ticks come more slowly when
-- your computer is busy, or just gets slow.


timeStep ( x, y ) ( dx, dy ) hasFriction =
    let
        friction =
            if hasFriction then
                0.994

            else
                1
    in
    ( x * friction + dx * s, y * friction + dy * s )



-- the update function records the time (in case we want animations)
-- and uses the timeStep function to change the two positions and two
-- velocities according to their definitions


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- TODO modify later
        Tick time ( _, _, ( deltaAngle, thrust ) ) ->
            let
                positiveThrust =
                    if thrust < 0 then
                        0

                    else
                        7 * thrust

                accel =
                    ( positiveThrust * cos -model.angle
                    , positiveThrust * sin -model.angle
                    )
            in
            { model
                | time = time
                , pos = timeStep model.pos model.vel False
                , vel = timeStep model.vel accel True
                , angle = model.angle + s * deltaAngle
            }


main =
    gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }


view model =
    collage 192 128 (myShapes model)
