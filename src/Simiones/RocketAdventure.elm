module Simiones.RocketAdventure exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


astroidShape =
    circle 12
        |> filled gray


astroid =
    group
        [ astroidShape
            |> addOutline (solid 1) black
        , astroidPat
            |> clip astroidShape
            |> addOutline (solid 1) black
        ]


astroidPat =
    group
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


shootingStar =
    group
        [ triangle 2
            |> filled (rgb 255 255 240)
            |> scaleX 4
            |> rotate (degrees 47)
            |> move ( -18.5, 2.7 )
        , circle 2
            |> filled (rgb 255 255 240)
            |> addOutline (solid 0.5) black
            |> move ( -21, 0 )
        ]


spaceBackground animation =
    group
        [ rect 200 200
            |> filled black
        , List.map
            (\x ->
                stars
                    |> scale (abs (1.5 * sin animation))
                    |> rotate (degrees 20 * animation)
                    |> move ( 80 * cos (20 * toFloat x + animation * 0.02), 80 * sin (toFloat x + animation * 0.02 + 1) )
            )
            (List.range 1 5)
            -- 10 shells
            |> group
        , List.map
            (\x ->
                stars
                    |> scale (abs (1.5 * sin animation))
                    |> rotate (degrees 20 * animation)
                    |> move ( 50 * cos (20 * toFloat x + animation * 0.02 + 2), 50 * sin (toFloat x + animation * 0.02 + 1.21) )
            )
            (List.range 1 5)
            -- 10 shells
            |> group
        , shootingStar
            |> move ( repeatDistance -100 300 100 animation, repeatDistance -100 300 80 animation )
        , shootingStar
            |> move ( repeatDistance -100 300 100 animation + 100, repeatDistance -100 300 80 animation + 100 )
            |> scaleX -1
        ]


stars =
    group
        [ triangle 1
            |> filled (rgb 255 255 240)
            |> scaleX 3
            |> rotate (degrees 90)
        , triangle 1
            |> filled (rgb 255 255 240)
            |> scaleX 3
        , triangle 1
            |> filled (rgb 255 255 240)
            |> scaleX 3
            |> rotate (degrees -90)
        , triangle 1
            |> filled (rgb 255 255 240)
            |> scaleX 3
            |> rotate (degrees 180)
        ]


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



-- repeat an animation for a given duration


repeatDuration : Float -> Int -> Float -> Float -> Float
repeatDuration speed duration startPosition time =
    speed * (time - toFloat duration * toFloat (floor time // duration)) + startPosition



-- repeat an animation for a given distance


repeatDistance : Float -> Float -> Float -> Float -> Float
repeatDistance speed distance startPosition time =
    repeatDuration speed (round <| distance / speed) startPosition time



-- sequence a series of animation pieces together into one


animationPieces : List ( Float, Float -> anytype ) -> (Float -> anytype) -> Float -> anytype
animationPieces intervals finalAnimation time =
    case intervals of
        ( duration, animation ) :: rest ->
            if time <= duration then
                animation time

            else
                animationPieces rest finalAnimation (time - duration)

        [] ->
            finalAnimation time
