module Sprites exposing (SpriteDef, all, palette)

{-| Every sprite in the game, drawn in code with the Pixel DSL and a
limited NES-style palette. Sprites face right; the renderer mirrors them.
-}

import Data
import Pixel exposing (..)
import Types exposing (Character(..))


type alias SpriteDef =
    { id : String, rows : List String, pal : List ( Char, String ) }


palette : List ( Char, String )
palette =
    [ ( 'k', "#101018" )
    , ( 'D', "#404048" )
    , ( 'g', "#7c7c84" )
    , ( 'G', "#bcbcc4" )
    , ( 'w', "#fcfcfc" )
    , ( 'r', "#d82800" )
    , ( 'R', "#881400" )
    , ( 'p', "#f878f8" )
    , ( 'o', "#fc9838" )
    , ( 'O', "#d85c10" )
    , ( 'y', "#fce4a0" )
    , ( 'Y', "#f8d000" )
    , ( 'n', "#ac7c00" )
    , ( 'b', "#503000" )
    , ( 't', "#a0582c" )
    , ( 's', "#fcc4a0" )
    , ( 'S', "#d08060" )
    , ( 'e', "#3cbcfc" )
    , ( 'E', "#0058f8" )
    , ( 'B', "#00288c" )
    , ( 'c', "#00e8d8" )
    , ( 'i', "#c4f0fc" )
    , ( 'v', "#9c6cfc" )
    , ( 'V', "#4c2cb0" )
    , ( 'l', "#b8f818" )
    , ( 'L', "#58d854" )
    , ( 'm', "#00a800" )
    , ( 'M', "#005800" )
    , ( 'f', "#fcfc58" )
    , ( 'F', "#fc7c00" )
    ]


sprite : String -> Canvas -> SpriteDef
sprite id cv =
    { id = id, rows = toRows cv, pal = [] }


all : List SpriteDef
all =
    players ++ characterSprites ++ auraSprites ++ weapons ++ enemies ++ items ++ projectiles ++ bosses ++ misc



-- PLAYERS (12x16) ----------------------------------------------------------


type Pose
    = Idle
    | Walk1
    | Walk2
    | Jump


hero : Pose -> Canvas
hero pose =
    let
        legs cv =
            case pose of
                Idle ->
                    cv |> rect 4 13 2 2 'D' |> rect 7 13 2 2 'D' |> rect 3 15 3 1 'b' |> rect 7 15 3 1 'b'

                Walk1 ->
                    cv |> rect 3 13 2 2 'D' |> rect 8 13 2 1 'D' |> rect 2 15 3 1 'b' |> rect 8 14 3 1 'b'

                Walk2 ->
                    cv |> rect 4 13 2 1 'D' |> rect 7 13 2 2 'D' |> rect 3 14 3 1 'b' |> rect 7 15 3 1 'b'

                Jump ->
                    cv |> rect 3 13 2 1 'D' |> rect 7 12 2 2 'D' |> rect 2 14 3 1 'b' |> rect 8 13 3 1 'b'
    in
    new 12 16
        |> legs
        |> rect 3 8 6 5 '1'
        |> rect 3 11 6 1 'b'
        |> dot 6 11 'Y'
        |> ellipse 6 4.6 3.7 3.6 '1'
        |> rect 5 4 5 3 's'
        |> dot 7 5 'k'
        |> dot 9 5 'k'
        |> dot 8 7 'S'
        |> rect 2 4 1 2 '2'
        |> rect 5 0 2 1 '2'
        |> rect 8 9 2 2 's'
        |> outline 'k'


ghost : Canvas
ghost =
    new 12 16
        |> ellipse 6 6 4.5 5 'w'
        |> rect 2 6 9 6 'w'
        |> dot 2 12 'w'
        |> dot 5 12 'w'
        |> dot 8 12 'w'
        |> dot 10 12 'w'
        |> dot 5 5 'k'
        |> dot 8 5 'k'
        |> rect 6 8 2 1 'k'
        |> outline '1'


players : List SpriteDef
players =
    List.concatMap
        (\i ->
            let
                d =
                    Data.playerDef i

                pal =
                    [ ( '1', d.color ), ( '2', d.dark ) ]

                mk name cv =
                    { id = "p" ++ String.fromInt i ++ "_" ++ name, rows = toRows cv, pal = pal }
            in
            [ mk "idle" (hero Idle), mk "walk1" (hero Walk1), mk "walk2" (hero Walk2), mk "jump" (hero Jump), mk "ghost" ghost ]
        )
        (List.range 0 3)



-- WEAPONS --------------------------------------------------------------------


weapons : List SpriteDef
weapons =
    [ sprite "w_fists" (new 5 5 |> rect 1 1 3 3 's' |> dot 3 2 'S' |> outline 'k')
    , sprite "w_fists_atk" (new 7 5 |> rect 3 1 3 3 's' |> rect 0 2 3 1 's' |> outline 'k')
    , sprite "w_nunchucks" (new 10 8 |> rect 1 1 2 5 't' |> dot 3 1 'G' |> dot 4 2 'G' |> rect 5 3 2 4 't' |> outline 'k')
    , sprite "w_nunchucks_atk" (new 14 8 |> rect 1 3 4 2 't' |> line 5 3 7 3 'G' |> rect 8 2 5 2 't' |> outline 'k')
    , sprite "w_sword" (new 14 6 |> rect 1 2 2 2 'b' |> rect 3 1 1 4 'Y' |> rect 4 2 8 2 'c' |> hline 4 2 8 'w' |> dot 12 3 'c' |> outline 'V')
    , sprite "w_sword_atk" (new 14 14 |> rect 1 1 2 2 'b' |> rect 3 2 2 1 'Y' |> rect 2 3 1 2 'Y' |> thickLine 4 4 11 11 2 'c' |> line 4 4 11 11 'w' |> outline 'V')
    , sprite "w_machinegun" (new 14 7 |> rect 1 2 8 3 'D' |> rect 9 3 4 1 'g' |> rect 3 5 2 2 'D' |> rect 6 1 3 1 'g' |> dot 2 3 'G' |> outline 'k')
    , sprite "w_bazooka" (new 16 7 |> rect 1 1 13 4 'm' |> hline 1 1 13 'l' |> rect 0 2 1 2 'M' |> rect 14 1 1 4 'D' |> rect 5 5 2 2 'D' |> rect 9 0 2 1 'r' |> outline 'k')
    , sprite "w_fartbomb" (new 9 10 |> ellipse 4.5 6 3.5 3.5 'L' |> ellipse 3.5 5 1.5 1.5 'l' |> dot 6 8 'M' |> rect 4 1 1 2 'b' |> dot 5 0 'f' |> outline 'k')
    , sprite "w_laser" (new 13 7 |> rect 1 2 8 3 'w' |> rect 9 3 3 1 'c' |> rect 3 5 2 2 'G' |> rect 2 3 4 1 'c' |> dot 8 1 'e' |> outline 'k')
    ]



-- ENEMIES --------------------------------------------------------------------


goon : Int -> Canvas
goon f =
    new 14 17
        |> (if f == 0 then
                rect 4 13 2 3 'D' >> rect 8 13 2 3 'D'

            else
                rect 3 13 2 3 'D' >> rect 9 13 2 2 'D'
           )
        |> rect 3 7 8 6 'O'
        |> rect 5 8 4 3 'Y'
        |> dot 7 9 'r'
        |> ellipse 7 4.5 3.5 3.5 's'
        |> rect 3 1 8 3 'Y'
        |> dot 5 0 'Y'
        |> dot 7 0 'Y'
        |> dot 9 0 'Y'
        |> rect 7 4 3 1 'k'
        |> dot 9 5 'k'
        |> rect 7 6 3 1 'R'
        |> rect 12 1 1 14 'b'
        |> rect 11 9 2 2 's'
        |> outline 'k'
        |> dot 12 0 'G'


babyDragon : Int -> Canvas
babyDragon f =
    new 18 14
        |> (if f == 0 then
                poly [ ( 6, 6 ), ( 3, 0 ), ( 11, 4 ) ] 'm'

            else
                poly [ ( 6, 7 ), ( 2, 12 ), ( 11, 8 ) ] 'm'
           )
        |> ellipse 7 8 5 3.5 'L'
        |> ellipse 7 9.5 3 1.5 'y'
        |> line 1 9 3 8 'L'
        |> dot 0 10 'L'
        |> ellipse 13 6 3.5 3 'L'
        |> dot 15 7 'L'
        |> dot 16 7 'L'
        |> rect 13 4 2 2 'w'
        |> dot 14 5 'k'
        |> dot 11 2 'y'
        |> rect 5 11 1 2 'L'
        |> rect 9 11 1 2 'L'
        |> outline 'k'


archer : Int -> Canvas
archer f =
    new 14 17
        |> rect 4 13 1 3 'w'
        |> rect 7 13 1 3 'w'
        |> rect 4 8 4 1 'w'
        |> rect 4 10 4 1 'w'
        |> rect 5 7 2 6 'G'
        |> ellipse 6 4 3.5 3.3 'w'
        |> rect 6 3 2 2 'k'
        |> dot 8 4 'k'
        |> rect 5 6 4 1 'G'
        |> (if f == 0 then
                line 10 3 12 7 'b' >> line 12 7 10 11 'b' >> line 10 3 10 11 'G'

            else
                line 10 3 12 7 'b' >> line 12 7 10 11 'b' >> line 10 3 8 7 'G' >> line 8 7 10 11 'G'
           )
        |> rect 8 8 2 1 'w'
        |> outline 'k'


enemies : List SpriteDef
enemies =
    [ sprite "e_goon_0" (goon 0)
    , sprite "e_goon_1" (goon 1)
    , sprite "e_baby_0" (babyDragon 0)
    , sprite "e_baby_1" (babyDragon 1)
    , sprite "e_archer_0" (archer 0)
    , sprite "e_archer_1" (archer 1)
    ]



-- ITEMS ----------------------------------------------------------------------


items : List SpriteDef
items =
    [ sprite "i_coin_0" (new 8 8 |> ellipse 4 4 3 3 'Y' |> rect 4 2 1 4 'n' |> dot 3 2 'y' |> outline 'n')
    , sprite "i_coin_1" (new 8 8 |> rect 3 1 2 6 'Y' |> dot 3 2 'y' |> outline 'n')
    , sprite "i_diamond" (new 9 8 |> poly [ ( 1, 3 ), ( 3, 1 ), ( 6, 1 ), ( 8, 3 ), ( 4.5, 8 ) ] 'c' |> rect 3 2 2 1 'w' |> line 2 3 4 6 'e' |> outline 'B')
    , sprite "i_heart" (new 9 8 |> ellipse 2.8 2.8 2 2 'r' |> ellipse 6.2 2.8 2 2 'r' |> poly [ ( 0.8, 3 ), ( 8.2, 3 ), ( 4.5, 7.5 ) ] 'r' |> dot 2 2 'w' |> outline 'R')
    , sprite "i_skull" (new 9 9 |> ellipse 4.5 3.5 3.5 3 'w' |> rect 3 6 3 2 'w' |> dot 3 3 'k' |> dot 6 3 'k' |> dot 4 7 'k' |> outline 'k')
    , sprite "i_crown" (new 11 8 |> rect 1 3 9 4 'Y' |> poly [ ( 1, 4 ), ( 1, 0 ), ( 4, 3 ) ] 'Y' |> poly [ ( 4, 3 ), ( 5.5, 0 ), ( 7, 3 ) ] 'Y' |> poly [ ( 7, 3 ), ( 10, 0 ), ( 10, 4 ) ] 'Y' |> dot 5 5 'r' |> dot 3 5 'c' |> dot 7 5 'c' |> outline 'n')
    ]



-- PROJECTILES ------------------------------------------------------------------


projectiles : List SpriteDef
projectiles =
    [ sprite "x_fireball" (new 8 8 |> ellipse 4 4 3.2 3.2 'F' |> ellipse 4.5 3.5 1.8 1.8 'f' |> outline 'r')
    , sprite "x_rock" (new 9 9 |> ellipse 4.5 4.5 3.5 3.3 'g' |> dot 3 3 'G' |> dot 5 6 'D' |> outline 'k')
    , sprite "x_spear" (new 14 4 |> hline 1 2 10 'b' |> poly [ ( 10, 0 ), ( 14, 2 ), ( 10, 4 ) ] 'G')
    , sprite "x_arrow" (new 11 3 |> hline 1 1 8 'b' |> dot 9 1 'G' |> dot 10 1 'G' |> dot 9 0 'G' |> dot 9 2 'G' |> dot 0 0 'w' |> dot 0 2 'w')
    , sprite "x_snowball" (new 8 8 |> ellipse 4 4 3 3 'w' |> dot 3 3 'i' |> outline 'e')
    , sprite "x_icicle" (new 6 10 |> poly [ ( 0, 0 ), ( 6, 0 ), ( 3, 10 ) ] 'i' |> line 2 1 3 7 'w' |> outline 'e')
    , sprite "x_bone" (new 10 5 |> hline 2 2 6 'w' |> rect 1 1 2 3 'w' |> rect 7 1 2 3 'w' |> outline 'G')
    , sprite "x_btc" (new 11 11 |> ellipse 5.5 5.5 4.5 4.5 'Y' |> rect 4 3 1 5 'b' |> hline 5 3 2 'b' |> dot 7 4 'b' |> hline 5 5 2 'b' |> rect 7 6 1 1 'b' |> hline 5 7 2 'b' |> dot 5 2 'b' |> dot 5 8 'b' |> dot 3 3 'b' |> dot 3 7 'b' |> outline 'n')
    , sprite "x_moneybag" (new 10 10 |> ellipse 5 6.5 3.5 3 't' |> rect 4 2 2 2 't' |> hline 3 3 4 'Y' |> rect 4 5 2 3 'Y' |> outline 'b')
    , sprite "x_rocket" (new 10 6 |> rect 1 2 6 2 'm' |> poly [ ( 7, 1.5 ), ( 10, 3 ), ( 7, 4.5 ) ] 'r' |> rect 1 1 2 1 'D' |> rect 1 4 2 1 'D' |> dot 0 2 'f' |> dot 0 3 'F')
    , sprite "x_fart" (new 8 8 |> ellipse 4 4 3 3 'L' |> dot 3 3 'l' |> dot 4 0 'b' |> outline 'M')
    , sprite "x_magic" (new 9 9 |> line 4 0 4 8 'v' |> line 0 4 8 4 'v' |> ellipse 4.5 4.5 2 2 'c' |> dot 4 4 'w')
    , sprite "x_wave" (new 12 10 |> poly [ ( 0, 10 ), ( 4, 2 ), ( 8, 0 ), ( 12, 10 ) ] 'G' |> poly [ ( 3, 10 ), ( 6, 4 ), ( 9, 10 ) ] 'g' |> outline 'D')
    , sprite "x_spark" (new 7 11 |> line 4 0 1 5 'f' |> line 1 5 5 5 'f' |> line 5 5 2 10 'f' |> line 3 1 2 4 'w' |> line 4 6 3 8 'w' |> outline 'c')
    ]



-- BOSSES -----------------------------------------------------------------------
-- frame 0/1 = idle animation, frame 2 = attacking


captain : Int -> Canvas
captain f =
    new 40 44
        |> rect 10 34 6 9 'D'
        |> rect 20 34 6 9 'D'
        |> rect
            (if f == 1 then
                8

             else
                9
            )
            41
            8
            3
            'b'
        |> rect 20 41 8 3 'b'
        |> ellipse 18 26 12 10 'O'
        |> rect 12 22 12 10 'Y'
        |> ellipse 18 26 3 3 'r'
        |> ellipse 19 11 8 8 's'
        |> rect 11 3 16 5 'G'
        |> rect 10 7 18 2 'g'
        |> rect 17 0 3 4 'r'
        |> rect 19 10 6 2 'k'
        |> dot 23 12 'k'
        |> dot 21 12 'w'
        |> rect
            (if f == 2 then
                19

             else
                20
            )
            15
            6
            (if f == 2 then
                2

             else
                1
            )
            'R'
        |> (if f == 2 then
                rect 26 22 6 4 's' >> hline 22 23 18 'b' >> poly [ ( 36, 21 ), ( 40, 23.5 ), ( 36, 26 ) ] 'G'

            else
                rect 27 22 4 4 's' >> rect 30 2 2 38 'b' >> poly [ ( 29, 4 ), ( 31, -1 ), ( 33, 4 ) ] 'G'
           )
        |> outline 'k'


brood : Int -> Canvas
brood f =
    new 60 40
        |> (if f == 1 then
                poly [ ( 20, 20 ), ( 6, 38 ), ( 34, 24 ) ] 'm'

            else
                poly [ ( 20, 18 ), ( 8, 0 ), ( 36, 14 ) ] 'm'
           )
        |> line 2 30 12 24 'L'
        |> line 2 31 12 25 'L'
        |> ellipse 24 24 14 8 'L'
        |> ellipse 25 28 9 3 'y'
        |> ellipse 45 16 9 7 'L'
        |> rect 50 15 8 5 'L'
        |> rect 46 11 4 3 'w'
        |> dot 48 12 'r'
        |> poly [ ( 40, 11 ), ( 37, 3 ), ( 43, 9 ) ] 'y'
        |> (if f == 2 then
                rect 51 19 8 2 'R' >> dot 52 18 'w' >> dot 56 18 'w'

            else
                hline 52 19 6 'M'
           )
        |> rect 18 30 3 5 'L'
        |> rect 28 30 3 5 'L'
        |> outline 'k'


golem : Int -> Canvas
golem f =
    new 48 52
        |> ellipse 14 44 7 6 'g'
        |> ellipse 33 44 7 6 'g'
        |> ellipse 24 30 18 13 'g'
        |> ellipse 18 26 6 4 'G'
        |> ellipse 30 35 5 3 'D'
        |> (if f == 2 then
                ellipse 42 22 6 7 'g' >> ellipse 44 10 5 5 'g'

            else
                ellipse 42 33 6 8 'g'
           )
        |> ellipse 7 33 6 8 'g'
        |> ellipse 25 12 10 9 'g'
        |> ellipse 22 9 4 3 'G'
        |> rect 26 11 3 2 'c'
        |> rect 32 11 3 2 'c'
        |> hline 27
            17
            (if f == 1 then
                6

             else
                7
            )
            'D'
        |> rect 14 20 6 3 'm'
        |> rect 30 26 5 2 'm'
        |> outline 'k'


bat : Int -> Canvas
bat f =
    let
        wings cv =
            if f == 1 then
                cv
                    |> poly [ ( 22, 14 ), ( 2, 30 ), ( 8, 26 ), ( 12, 32 ), ( 18, 26 ), ( 24, 22 ) ] 'V'
                    |> poly [ ( 38, 14 ), ( 58, 30 ), ( 52, 26 ), ( 48, 32 ), ( 42, 26 ), ( 36, 22 ) ] 'V'

            else
                cv
                    |> poly [ ( 22, 16 ), ( 0, 2 ), ( 6, 10 ), ( 4, 16 ), ( 12, 16 ), ( 14, 22 ), ( 24, 22 ) ] 'V'
                    |> poly [ ( 38, 16 ), ( 60, 2 ), ( 54, 10 ), ( 56, 16 ), ( 48, 16 ), ( 46, 22 ), ( 36, 22 ) ] 'V'
    in
    new 60 34
        |> wings
        |> ellipse 30 20 9 10 'v'
        |> poly [ ( 22, 14 ), ( 30, 32 ), ( 38, 14 ) ] 'R'
        |> ellipse 30 11 7 6 'v'
        |> poly [ ( 24, 8 ), ( 24, 1 ), ( 28, 6 ) ] 'v'
        |> poly [ ( 36, 8 ), ( 36, 1 ), ( 32, 6 ) ] 'v'
        |> rect 27 10 2 2 'r'
        |> rect 32 10 2 2 'r'
        |> (if f == 2 then
                rect 28 14 5 3 'k' >> dot 28 14 'w' >> dot 32 14 'w'

            else
                hline 28 14 5 'k' >> dot 28 15 'w' >> dot 32 15 'w'
           )
        |> outline 'k'


troll : Int -> Canvas
troll f =
    new 48 50
        |> rect 12 40 7 9 'm'
        |> rect 27 40 7 9 'm'
        |> ellipse 23 31 14 12 'L'
        |> rect 12 36 22 6 't'
        |> ellipse 23 14 10 9 'L'
        |> ellipse 31 16 4 3 'm'
        |> rect 14 0 16 6 'k'
        |> rect 11 6 22 2 'k'
        |> hline 14 5 16 'Y'
        |> rect 23 10 3 2 'w'
        |> dot 25 11 'k'
        |> rect 22 9 5 1 'M'
        |> hline 23
            (if f == 2 then
                19

             else
                20
            )
            8
            'k'
        |> dot 25 19 'w'
        |> dot 29 19 'w'
        |> (if f == 2 then
                ellipse 40 18 6 6 't' >> rect 38 16 4 4 'Y'

            else
                ellipse 39 33 6 6 't' >> rect 37 31 4 4 'Y'
           )
        |> ellipse 6 31 4 6 'L'
        |> outline 'k'


yeti : Int -> Canvas
yeti f =
    new 50 52
        |> rect 12 42 9 9 'G'
        |> rect 28 42 9 9 'G'
        |> ellipse 25 30 17 15 'w'
        |> ellipse 25 33 9 9 'G'
        |> (if f == 2 then
                ellipse 43 14 6 8 'w' >> ellipse 44 6 5 4 'w'

            else
                ellipse 44 32 6 10 'w'
           )
        |> ellipse 6 32 6 10 'w'
        |> ellipse 26 12 11 10 'w'
        |> ellipse 29 14 7 6 'e'
        |> rect 25 10 3 2 'k'
        |> rect 31 10 3 2 'k'
        |> hline 24 9 5 'B'
        |> hline 30 9 5 'B'
        |> (if f == 2 then
                rect 26 16 7 4 'k' >> dot 27 16 'w' >> dot 31 16 'w'

            else
                hline 27
                    17
                    (if f == 1 then
                        4

                     else
                        6
                    )
                    'B'
           )
        |> outline 'k'


{-| Crypto Chad, tech bro: hoodie, slick hair, shades (laser eyes when
attacking), phone with an "up only" chart and an energy drink.
-}
cryptoBro : Int -> Canvas
cryptoBro f =
    new 36 46
        |> rect 12 34 5 9 'E'
        |> rect 19 34 5 9 'E'
        |> rect
            (if f == 1 then
                10

             else
                11
            )
            42
            7
            3
            'w'
        |> rect 19 42 7 3 'w'
        |> ellipse 18 27 10 9 'g'
        |> rect 9 24 18 11 'g'
        |> line 18 20 18 34 'G'
        |> hline 12 31 12 'D'
        |> dot 22 24 'F'
        |> ellipse 18 14 9 8 'g'
        |> ellipse 19 12 7 7 's'
        |> rect 12 4 14 4 'b'
        |> poly [ ( 12, 6 ), ( 28, 3 ), ( 26, 8 ) ] 'b'
        |> hline 15 10 12 'k'
        |> rect 17 10 5 3 'k'
        |> rect 23 10 4 3 'k'
        |> (if f == 2 then
                rect 18 11 3 1 'r' >> rect 24 11 2 1 'r'

            else
                dot 18 10 'G' >> dot 24 10 'G'
           )
        |> (if f == 2 then
                rect 21 16 4 2 'k' >> hline 21 16 4 'w'

            else
                hline 21 16 4 'R' >> dot 25 15 'R'
           )
        |> (if f == 2 then
                ellipse 30 14 3 5 'g' >> rect 28 3 6 9 'D' >> line 29 10 33 5 'L' >> dot 33 5 'l'

            else
                ellipse 29 28 3 6 'g' >> rect 28 31 6 8 'D' >> line 29 37 33 32 'L'
           )
        |> ellipse 7 28 3 6 'g'
        |> rect 4 29 3 6 'l'
        |> hline 4 29 3 'G'
        |> outline 'k'


sala : Int -> Canvas
sala f =
    new 66 32
        |> thickLine 1 18 12 22 3 'r'
        |> ellipse 30 20 20 8 'r'
        |> ellipse 30 24 14 3 'F'
        |> ellipse 24 16 3 2 'f'
        |> ellipse 34 15 2 2 'f'
        |> ellipse 18 19 2 2 'f'
        |> ellipse 54 15 9 6 'r'
        |> rect 56 13 9 5 'r'
        |> rect 54 11 3 3 'f'
        |> dot 55 12 'k'
        |> (if f == 2 then
                rect 57 17 8 3 'k' >> hline 58 17 6 'f'

            else
                hline 57 17 8 'R'
           )
        |> rect
            (if f == 1 then
                18

             else
                20
            )
            26
            4
            5
            'r'
        |> rect
            (if f == 1 then
                40

             else
                38
            )
            26
            4
            5
            'r'
        |> outline 'k'


cyclops : Int -> Canvas
cyclops f =
    new 52 60
        |> rect 14 48 8 11 'v'
        |> rect 30 48 8 11 'v'
        |> rect 12 57 10 3 'b'
        |> rect 30 57 10 3 'b'
        |> ellipse 26 34 17 15 'v'
        |> rect 12 40 28 8 't'
        |> hline 12 40 28 'b'
        |> ellipse 26 13 12 11 'v'
        |> ellipse 26 12 6 5 'w'
        |> ellipse
            (if f == 2 then
                27

             else
                26
            )
            12
            3
            3
            'r'
        |> dot
            (if f == 2 then
                27

             else
                26
            )
            12
            'k'
        |> hline 19 5 15 'V'
        |> hline 21
            (if f == 2 then
                19

             else
                20
            )
            10
            'V'
        |> poly [ ( 23, 1 ), ( 26, -4 ), ( 29, 1 ) ] 'G'
        |> ellipse 7 34 5 9 'v'
        |> (if f == 2 then
                ellipse 45 22 5 7 'v' >> thickLine 45 18 50 2 4 'b'

            else
                ellipse 45 34 5 9 'v' >> thickLine 45 40 50 56 4 'b'
           )
        |> outline 'k'


{-| The greedy, orange, old Evil King.
-}
king : Int -> Canvas
king f =
    new 60 60
        |> rect 18 50 9 7 'V'
        |> rect 33 50 9 7 'V'
        |> rect 16 56 11 4 'b'
        |> rect 33 56 11 4 'b'
        |> ellipse 30 33 26 20 'R'
        |> hline 5 33 50 'w'
        |> ellipse 30 37 20 15 'o'
        |> ellipse 30 44 16 6 'O'
        |> dot 31 40 'O'
        |> rect 12 50 36 2 'Y'
        |> dot 30 50 'r'
        |> ellipse 32 15 12 11 'o'
        |> ellipse 32 22 9 4 'O'
        |> hline 26 9 4 'O'
        |> hline 34 9 4 'O'
        |> rect 27 12 4 2 'G'
        |> rect 35 12 5 2 'G'
        |> dot 29 14 'k'
        |> dot 37 14 'k'
        |> ellipse 34 17 2 2 'O'
        |> rect 29 19 10 2 'G'
        |> (if f == 2 then
                rect 30 21 8 4 'k' >> rect 32 23 4 2 'r'

            else
                hline 31 22 6 'R' >> dot 30 23 'R' >> dot 37 23 'R'
           )
        -- big blonde combover wig, swooping forward under the crown
        |> ellipse 31 7 14 5 'f'
        |> rect 17 7 6 9 'f'
        |> poly [ ( 36, 4 ), ( 51, 5 ), ( 46, 11 ), ( 34, 10 ) ] 'f'
        |> line 19 8 48 7 'Y'
        |> line 18 11 44 10 'Y'
        |> line 18 14 22 14 'Y'
        |> rect 22 0 20 5 'Y'
        |> poly [ ( 22, 1 ), ( 22, -4 ), ( 26, 1 ) ] 'Y'
        |> poly [ ( 29, 1 ), ( 32, -5 ), ( 35, 1 ) ] 'Y'
        |> poly [ ( 38, 1 ), ( 42, -4 ), ( 42, 1 ) ] 'Y'
        |> dot 26 2 'r'
        |> dot 32 2 'c'
        |> dot 38 2 'r'
        |> (if f == 2 then
                ellipse 52 20 5 5 'o' >> ellipse 54 11 5 5 'Y' >> rect 53 10 2 3 'n'

            else
                ellipse 52 38 5 7 'o' >> ellipse 54 46 6 5 'Y' >> rect 53 44 2 3 'n'
           )
        |> ellipse 7 38 5 7 'o'
        |> outline 'k'


{-| Diablo, the mega dragon. frame: 0/1 wings up/down, 2 mouth open.
-}
dragon : Int -> Canvas
dragon f =
    let
        wing cv =
            if f == 1 then
                cv
                    |> poly [ ( 34, 34 ), ( 50, 30 ), ( 30, 70 ), ( 24, 58 ), ( 16, 62 ), ( 18, 48 ) ] 'V'
                    |> line 34 34 30 68 'v'
                    |> line 34 34 18 50 'v'

            else
                cv
                    |> poly [ ( 30, 34 ), ( 52, 32 ), ( 60, 2 ), ( 44, 12 ), ( 36, 4 ), ( 30, 14 ), ( 18, 8 ) ] 'V'
                    |> line 40 32 60 2 'v'
                    |> line 36 32 36 5 'v'
                    |> line 32 32 18 8 'v'
    in
    new 98 74
        |> thickLine 2 60 22 50 4 'R'
        |> poly [ ( 0, 58 ), ( 4, 52 ), ( 6, 64 ) ] 'r'
        |> rect 28 58 9 14 'R'
        |> rect 52 58 9 14 'R'
        |> hline 27 72 11 'w'
        |> hline 51 72 11 'w'
        |> ellipse 44 50 24 15 'R'
        |> ellipse 48 56 16 7 'o'
        |> hline 38 54 20 'O'
        |> hline 40 58 16 'O'
        |> thickLine 60 46 74 26 9 'R'
        |> wing
        |> ellipse 80 24 11 9 'R'
        |> (if f == 2 then
                rect 84 22 13 4 'R' >> rect 84 28 12 4 'R' >> rect 86 26 11 2 'F' >> dot 92 26 'f' >> dot 88 25 'w' >> dot 93 25 'w'

            else
                rect 84 22 13 8 'R' >> hline 86 27 10 'k' >> dot 89 28 'w' >> dot 94 28 'w'
           )
        |> dot 95 23 'k'
        |> poly [ ( 72, 18 ), ( 66, 2 ), ( 76, 15 ) ] 'G'
        |> poly [ ( 78, 15 ), ( 78, 3 ), ( 82, 15 ) ] 'G'
        |> rect 84 18 3 3 'r'
        |> rect 78 18 3 3 'r'
        |> dot 85 19 'f'
        |> dot 79 19 'f'
        |> outline 'k'


bosses : List SpriteDef
bosses =
    let
        frames name fn =
            List.map (\i -> sprite (name ++ "_" ++ String.fromInt i) (fn i)) [ 0, 1, 2 ]
    in
    frames "b_captain" captain
        ++ frames "b_brood" brood
        ++ frames "b_golem" golem
        ++ frames "b_bat" bat
        ++ frames "b_troll" troll
        ++ frames "b_yeti" yeti
        ++ frames "b_crypto" cryptoBro
        ++ frames "b_sala" sala
        ++ frames "b_cyclops" cyclops
        ++ frames "b_king" king
        ++ frames "b_dragon" dragon



-- MISC -------------------------------------------------------------------------


misc : List SpriteDef
misc =
    [ sprite "king_portrait" (king 2)
    , sprite "dragon_title" (dragon 2)
    , sprite "warn" (new 7 7 |> poly [ ( 3.5, 0 ), ( 7, 7 ), ( 0, 7 ) ] 'Y' |> rect 3 2 1 3 'k' |> dot 3 6 'k')
    ]



-- CHARACTERS (about 12x16, facing right) --------------------------------------------


{-| Two legs with feet. lx/rx are the leg columns in the idle pose.
-}
legsAt : Pose -> Char -> Char -> Int -> Int -> Canvas -> Canvas
legsAt pose leg foot lx rx cv =
    case pose of
        Idle ->
            cv |> rect lx 13 2 2 leg |> rect rx 13 2 2 leg |> rect (lx - 1) 15 3 1 foot |> rect rx 15 3 1 foot

        Walk1 ->
            cv |> rect (lx - 1) 13 2 2 leg |> rect (rx + 1) 13 2 1 leg |> rect (lx - 2) 15 3 1 foot |> rect (rx + 1) 14 3 1 foot

        Walk2 ->
            cv |> rect lx 13 2 1 leg |> rect rx 13 2 2 leg |> rect (lx - 1) 14 3 1 foot |> rect rx 15 3 1 foot

        Jump ->
            cv |> rect (lx - 1) 13 2 1 leg |> rect rx 12 2 2 leg |> rect (lx - 2) 14 3 1 foot |> rect (rx + 1) 13 3 1 foot


goblin : Pose -> Canvas
goblin pose =
    new 13 16
        |> legsAt pose 'm' 'b' 4 7
        |> rect 3 9 7 4 't'
        |> rect 3 11 7 1 'b'
        |> dot 3 13 't'
        |> dot 9 13 't'
        |> rect 9 9 2 2 'L'
        |> ellipse 6.5 5.5 3.6 3.4 'L'
        |> poly [ ( 3.5, 4 ), ( 0, 2 ), ( 3, 7 ) ] 'L'
        |> poly [ ( 9.5, 4 ), ( 13, 2 ), ( 10, 7 ) ] 'L'
        |> dot 7 5 'Y'
        |> dot 9 5 'Y'
        |> hline 6 8 4 'M'
        |> dot 7 8 'w'
        |> dot 9 8 'w'
        |> outline 'k'


fairy : Pose -> Canvas
fairy pose =
    let
        flap =
            pose == Walk1 || pose == Jump
    in
    new 12 16
        |> (if flap then
                poly [ ( 4, 7 ), ( 0, 1 ), ( 1, 8 ) ] 'i' >> poly [ ( 4, 9 ), ( 0, 13 ), ( 2, 13 ) ] 'e'

            else
                poly [ ( 4, 7 ), ( 0, 4 ), ( 1, 9 ) ] 'i' >> poly [ ( 4, 9 ), ( 1, 12 ), ( 3, 12 ) ] 'e'
           )
        |> legsAt pose 's' 'p' 4 7
        |> poly [ ( 4, 8 ), ( 8, 8 ), ( 10, 13 ), ( 2, 13 ) ] 'L'
        |> hline 3 12 7 'm'
        |> rect 8 9 2 2 's'
        |> ellipse 6.5 4.5 3.5 3.3 'O'
        |> rect 5 4 5 3 's'
        |> dot 7 5 'k'
        |> dot 9 5 'k'
        |> dot 8 6 'p'
        |> outline 'k'
        |> dot 6 0 'c'


{-| A sparkly pop star with long golden hair.
-}
popStar : Pose -> Canvas
popStar pose =
    new 12 16
        |> rect 2 4 3 7 'Y'
        |> legsAt pose 's' 'w' 4 7
        |> rect 4 8 5 4 'v'
        |> poly [ ( 3, 11 ), ( 9, 11 ), ( 10, 13 ), ( 2, 13 ) ] 'v'
        |> dot 5 9 'w'
        |> dot 7 10 'w'
        |> dot 4 12 'w'
        |> dot 8 12 'w'
        |> rect 8 8 2 3 's'
        |> ellipse 6.5 4.2 3.5 3.4 'Y'
        |> rect 5 3 5 4 's'
        |> hline 5 3 5 'Y'
        |> dot 7 4 'k'
        |> dot 9 4 'k'
        |> dot 8 6 'r'
        |> outline 'k'


{-| A friendly green dinosaur with a saddle.
-}
dino : Pose -> Canvas
dino pose =
    new 15 16
        |> legsAt pose 'L' 'O' 4 8
        |> poly [ ( 3, 10 ), ( 0, 14 ), ( 5, 12 ) ] 'L'
        |> ellipse 6.5 10 4.2 3.5 'L'
        |> ellipse 8 11 2.3 2 'w'
        |> rect 3 7 4 2 'r'
        |> dot 2 9 'r'
        |> rect 9 5 3 5 'L'
        |> ellipse 10 4 3.2 3 'L'
        |> ellipse 12.5 5.5 2.4 1.8 'L'
        |> rect 9 1 2 3 'w'
        |> dot 10 2 'k'
        |> dot 14 5 'm'
        |> hline 11 7 3 'm'
        |> rect 11 9 2 1 'L'
        |> outline 'k'


treeMan : Pose -> Canvas
treeMan pose =
    new 13 16
        |> legsAt pose 'b' 'b' 4 7
        |> rect 4 6 5 7 't'
        |> dot 4 11 'b'
        |> dot 8 7 'b'
        |> dot 8 12 'b'
        |> dot 5 8 'f'
        |> dot 7 8 'f'
        |> hline 5 10 3 'b'
        |> line 8 9 11 6 'b'
        |> dot 12 5 'L'
        |> line 4 9 1 7 'b'
        |> ellipse 6.5 3.5 5 3.4 'm'
        |> ellipse 5 2.5 2 1.5 'L'
        |> ellipse 9 3.5 1.8 1.2 'L'
        |> dot 3 4 'L'
        |> outline 'k'


{-| A lady with a cherry pie for a head (and plenty more to throw).
-}
pieWoman : Pose -> Canvas
pieWoman pose =
    new 12 16
        |> legsAt pose 'S' 'r' 4 7
        |> rect 3 9 7 4 'p'
        |> rect 5 9 3 4 'w'
        |> rect 9 9 2 2 'S'
        |> ellipse 6.5 5 4.6 3.3 'o'
        |> hline 3 3 8 'r'
        |> hline 4 2 6 'r'
        |> dot 5 2 'R'
        |> dot 8 3 'R'
        |> dot 3 6 'O'
        |> dot 10 6 'O'
        |> dot 7 5 'k'
        |> dot 9 5 'k'
        |> hline 7 7 2 'R'
        |> outline 'k'
        |> dot 6 1 'w'
        |> dot 7 1 'w'


{-| BlackBerry the black dog.
-}
dog : Pose -> Canvas
dog pose =
    let
        legs =
            case pose of
                Idle ->
                    [ 3, 5, 10, 12 ]

                Walk1 ->
                    [ 2, 6, 9, 13 ]

                Walk2 ->
                    [ 4, 4, 11, 11 ]

                Jump ->
                    [ 2, 3, 13, 14 ]

        legH =
            if pose == Jump then
                2

            else
                3

        tail cv =
            case pose of
                Walk1 ->
                    line 3 5 0 3 'D' cv

                Walk2 ->
                    line 3 5 1 1 'D' cv

                _ ->
                    line 3 5 1 2 'D' cv
    in
    new 17 12
        |> tail
        |> (\cv -> List.foldl (\lx acc -> rect lx 8 2 legH 'D' acc) cv legs)
        |> ellipse 8 6 5.5 3 'D'
        |> hline 6 4 4 'g'
        |> ellipse 13 3.8 3 2.8 'D'
        |> rect 14 4 3 2 'D'
        |> dot 16 4 'k'
        |> dot 14 3 'w'
        |> dot 15 6 'p'
        |> poly [ ( 11, 1 ), ( 13, 1 ), ( 11, 5 ) ] 'k'
        |> rect 11 6 1 2 'r'
        |> dot 12 7 'Y'
        |> outline 'k'


blockMan : Pose -> Canvas
blockMan pose =
    new 12 16
        |> legsAt pose 'D' 'D' 4 7
        |> rect 3 7 7 6 'g'
        |> hline 3 9 7 'D'
        |> line 6 7 6 12 'D'
        |> rect 9 8 2 3 'G'
        |> rect 3 0 7 7 'G'
        |> hline 3 2 7 'g'
        |> dot 6 4 'k'
        |> dot 8 4 'k'
        |> hline 6 6 3 'D'
        |> outline 'k'


{-| Mr. Poopy Diaper, a baby with a very full diaper.
-}
poopyDiaper : Pose -> Canvas
poopyDiaper pose =
    new 12 16
        |> legsAt pose 's' 's' 4 7
        |> rect 3 10 7 3 'w'
        |> dot 4 12 'b'
        |> dot 5 11 'b'
        |> dot 6 12 'b'
        |> dot 8 11 'y'
        |> rect 4 8 5 2 's'
        |> rect 9 8 2 2 's'
        |> ellipse 6.5 4.6 4 3.8 's'
        |> dot 6 4 'w'
        |> dot 7 4 'k'
        |> dot 8 4 'w'
        |> dot 9 4 'k'
        |> dot 5 6 'p'
        |> dot 10 6 'p'
        |> dot 8 7 'e'
        |> outline 'k'
        |> dot 6 0 'b'
        |> dot 7 1 'b'


ghosty : Pose -> Canvas
ghosty pose =
    let
        hem =
            case pose of
                Walk1 ->
                    [ 1, 4, 7, 10 ]

                Walk2 ->
                    [ 2, 5, 8 ]

                Jump ->
                    [ 3, 6, 9 ]

                Idle ->
                    [ 2, 5, 8, 10 ]
    in
    new 12 16
        |> ellipse 6 6.5 4.5 5 'i'
        |> rect 2 6 9 7 'i'
        |> (\cv -> List.foldl (\x acc -> dot x 13 'i' acc) cv hem)
        |> hline 2 12 9 'e'
        |> dot 11 8 'i'
        |> rect 5 4 2 3 'k'
        |> rect 8 4 2 3 'k'
        |> rect 7 9 2 2 'k'
        |> outline 'k'


beerMan : Pose -> Canvas
beerMan pose =
    new 13 16
        |> legsAt pose 'D' 'b' 5 8
        |> rect 3 4 7 9 'Y'
        |> line 4 5 4 11 'y'
        |> hline 3 12 7 'n'
        |> rect 0 6 3 1 'G'
        |> rect 0 6 1 4 'G'
        |> rect 0 9 3 1 'G'
        |> ellipse 6.5 3.5 4.2 2 'w'
        |> dot 9 5 'w'
        |> dot 6 7 'k'
        |> dot 8 7 'k'
        |> hline 6 9 3 'n'
        |> rect 10 8 2 1 'y'
        |> outline 'k'
        |> dot 4 0 'w'
        |> dot 8 0 'w'


spider : Pose -> Canvas
spider pose =
    let
        k =
            case pose of
                Walk1 ->
                    1

                Walk2 ->
                    -1

                _ ->
                    0

        lift =
            if pose == Jump then
                3

            else
                0

        leg i fx =
            let
                wobble =
                    if modBy 2 i == 0 then
                        k

                    else
                        -k

                kneeY =
                    if modBy 2 i == 0 then
                        2

                    else
                        4
            in
            line ((fx + 8) // 2) 6 fx kneeY 'D' >> line fx kneeY (fx + wobble) (11 - lift) 'D'
    in
    List.foldl (\( i, fx ) cv -> leg i fx cv) (new 17 12) (List.indexedMap Tuple.pair [ 1, 3, 5, 7, 9, 11, 13, 15 ])
        |> ellipse 5.5 6 4 3.5 'v'
        |> hline 3 5 4 'V'
        |> hline 3 7 4 'V'
        |> ellipse 11 6.5 3 2.5 'v'
        |> dot 12 5 'w'
        |> dot 13 6 'w'
        |> dot 11 5 'r'
        |> dot 14 8 'w'
        |> outline 'k'


characterSprites : List SpriteDef
characterSprites =
    let
        drawer c =
            case c of
                Goblin ->
                    goblin

                Fairy ->
                    fairy

                PopStar ->
                    popStar

                Dino ->
                    dino

                TreeMan ->
                    treeMan

                PieWoman ->
                    pieWoman

                BlackBerry ->
                    dog

                BlockMan ->
                    blockMan

                PoopyDiaper ->
                    poopyDiaper

                Ghosty ->
                    ghosty

                BeerMan ->
                    beerMan

                Spider ->
                    spider
    in
    List.concatMap
        (\c ->
            let
                key =
                    (Data.characterInfo c).key
            in
            List.map (\( name, pose ) -> sprite ("c_" ++ key ++ "_" ++ name) (drawer c pose))
                [ ( "idle", Idle ), ( "walk1", Walk1 ), ( "walk2", Walk2 ), ( "jump", Jump ) ]
        )
        Data.characters



-- AURAS ------------------------------------------------------------------------------


auraSprites : List SpriteDef
auraSprites =
    [ sprite "a_star" (new 5 5 |> hline 0 2 5 'p' |> rect 2 0 1 5 'p' |> dot 2 2 'w')
    , sprite "a_star2" (new 5 5 |> hline 0 2 5 'c' |> rect 2 0 1 5 'c' |> dot 2 2 'w')
    , sprite "a_note" (new 5 7 |> rect 3 0 1 5 'p' |> rect 3 0 2 1 'p' |> dot 4 1 'p' |> rect 1 4 3 2 'p' |> rect 0 5 2 2 'p')
    , sprite "a_note2" (new 5 7 |> rect 3 0 1 5 'v' |> rect 3 0 2 1 'v' |> dot 4 1 'v' |> rect 1 4 3 2 'v' |> rect 0 5 2 2 'v')
    , sprite "a_footprint" (new 5 3 |> rect 1 1 3 2 'm' |> dot 0 0 'm' |> dot 2 0 'm' |> dot 4 0 'm')
    , sprite "a_leaf" (new 4 3 |> hline 1 0 2 'L' |> hline 0 1 4 'L' |> hline 1 2 2 'm')
    , sprite "a_leaf2" (new 4 3 |> hline 1 0 2 'o' |> hline 0 1 4 'O' |> hline 1 2 2 'O')
    , sprite "a_splat" (new 7 3 |> hline 1 1 5 'w' |> hline 0 2 7 'y' |> dot 2 0 'w' |> dot 5 0 'r')
    , sprite "a_paw" (new 5 4 |> rect 1 2 3 2 'g' |> dot 0 1 'g' |> dot 2 0 'g' |> dot 4 1 'g')
    , sprite "a_heart" (new 5 4 |> dot 1 0 'r' |> dot 3 0 'r' |> hline 0 1 5 'r' |> hline 1 2 3 'r' |> dot 2 3 'r')
    , sprite "a_block" (new 3 3 |> rect 0 0 3 3 'g' |> dot 0 0 'G')
    , sprite "a_stink" (new 3 7 |> dot 1 0 'l' |> dot 0 1 'l' |> dot 1 2 'n' |> dot 2 3 'n' |> dot 1 4 'l' |> dot 0 5 'l' |> dot 1 6 'n')
    , sprite "a_bubble" (new 4 4 |> hline 1 0 2 'w' |> hline 1 3 2 'w' |> rect 0 1 1 2 'w' |> rect 3 1 1 2 'w' |> dot 1 1 'y')
    , sprite "a_web" (new 11 7 |> line 5 0 0 6 'G' |> line 5 0 10 6 'G' |> line 5 0 5 6 'G' |> line 5 0 2 6 'G' |> line 5 0 8 6 'G' |> hline 3 2 5 'w' |> hline 1 4 9 'w' |> hline 0 6 11 'G')
    , sprite "a_fly" (new 3 2 |> dot 1 1 'k' |> dot 0 0 'i' |> dot 2 0 'i')
    , sprite "a_crumb" (new 2 2 |> rect 0 0 2 2 'o')
    ]
