module Nagii.RocketMoving exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


myShapes model =
    [ spaceBackground model.time
    , rocket blue model.time
        |> scale 0.25
        |> rotate (degrees 90)
        |> animationPieces
            [ ( 3, start )
            , ( 4, \t -> start 3 >> trans1 t )
            , ( 1, \t -> start 3 >> trans1 4 >> trans2 t )
            , ( 4, \t -> start 3 >> trans1 4 >> trans2 1 >> trans3 t )
            , ( 4, \t -> start 3 >> trans1 4 >> trans2 1 >> trans3 4 >> trans4 t )
            , ( 1, \t -> start 3 >> trans1 4 >> trans2 1 >> trans3 4 >> trans4 4 >> trans5 t )
            , ( 1, \t -> start 3 >> trans1 4 >> trans2 1 >> trans3 4 >> trans4 4 >> trans5 1 >> trans6 t )
            , ( 6, \t -> start 3 >> trans1 4 >> trans2 1 >> trans3 4 >> trans4 4 >> trans5 1 >> trans6 1 >> trans7 t )
            , ( 6, \t -> start 3 >> trans1 4 >> trans2 1 >> trans3 4 >> trans4 4 >> trans5 1 >> trans6 1 >> trans7 6 >> trans8 t )
            ]
            (\t -> start 3 >> trans1 4 >> trans2 1 >> trans3 4 >> trans4 4 >> trans5 1 >> trans6 1 >> trans7 6 >> trans8 t)
            model.time
    ]


start t =
    move ( -19 * t + 60, 50 )


trans1 t =
    move ( -5, 0 ) >> rotate (pi / 2 * t) >> move ( 5, 0 )


trans2 t =
    move ( -2, 0 ) >> rotate (pi / 2 * t) >> move ( 2, 0 )


trans3 t =
    move ( 40, 0 ) >> rotate (pi / 2 * t) >> move ( -40, 0 )


trans4 t =
    move ( 68, 0 ) >> rotate (-pi / 1 * t) >> move ( -68, 0 )


trans5 t =
    move ( -2, 0 ) >> rotate (pi / 2 * t) >> move ( 2, 0 )


trans6 t =
    move ( 5 * t, 0 )


trans7 t =
    move ( -17, 0 ) >> rotate (pi / 4 * t) >> move ( 17, 0 )


trans8 t =
    move ( 0, 0 ) >> rotate (pi / 3 * t) >> move ( 0, 0 )


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
                    |> move ( 80 * sin (10 * toFloat x + animation * 0.02) + 50 * cos (5 * toFloat x + animation * 0.04), 80 * sin (10 + 20 * toFloat x + animation * 0.02) + 50 * cos (5 * toFloat x + animation * 0.04) )
            )
            (List.range 1 10)
            -- 10 shells
            |> group
        , List.map
            (\x ->
                stars
                    |> scale (abs (1.5 * sin animation))
                    |> rotate (degrees 20 * animation)
                    |> move ( 80 * cos (10 * toFloat x + animation * 0.02) + 50 * sin (5 * toFloat x + animation * 0.04), 80 * cos (10 + 20 * toFloat x + animation * 0.02) + 50 * sin (5 * toFloat x + animation * 0.04) )
            )
            (List.range 1 10)
            -- 10 shells
            |> group
        , shootingStar
            |> move ( repeatDistance -100 300 100 animation, repeatDistance -100 300 80 animation )
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
