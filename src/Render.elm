module Render exposing
    ( Draw
    , characterSelect
    , encode
    , gameOver
    , interlude
    , story
    , title
    , victory
    , world
    )

{-| Turns game state into a flat list of draw commands. The TypeScript
platform layer rasterises them onto a 384x240 pixel canvas.
-}

import Boss
import Data
import Json.Encode as E
import Types exposing (..)
import Util exposing (..)
import World


type Anchor
    = TopLeft
    | BottomCenter
    | Center


type Draw
    = Rect Float Float Float Float String
    | Spr String Float Float Anchor Bool Bool Int
    | Text String Float Float String Align Int
    | Circ Float Float Float String Bool
    | Line Float Float Float Float Float String
    | Alpha Float


type Align
    = Left
    | Mid
    | Right


encode : List Draw -> E.Value
encode draws =
    E.list encodeDraw draws


encodeDraw : Draw -> E.Value
encodeDraw d =
    let
        f v =
            E.float (toFloat (round v))

        b v =
            E.int
                (if v then
                    1

                 else
                    0
                )
    in
    case d of
        Rect x y w h c ->
            E.list identity [ E.string "r", f x, f y, f w, f h, E.string c ]

        Spr id x y anchor flip flash scale ->
            E.list identity
                [ E.string "s"
                , E.string id
                , f x
                , f y
                , E.int
                    (case anchor of
                        TopLeft ->
                            0

                        BottomCenter ->
                            1

                        Center ->
                            2
                    )
                , b flip
                , b flash
                , E.int scale
                ]

        Text s x y c align scale ->
            E.list identity
                [ E.string "t"
                , E.string s
                , f x
                , f y
                , E.string c
                , E.int
                    (case align of
                        Left ->
                            0

                        Mid ->
                            1

                        Right ->
                            2
                    )
                , E.int scale
                ]

        Circ x y r c filled ->
            E.list identity [ E.string "c", f x, f y, f r, E.string c, b filled ]

        Line x1 y1 x2 y2 t c ->
            E.list identity [ E.string "l", f x1, f y1, f x2, f y2, f t, E.string c ]

        Alpha a ->
            E.list identity [ E.string "a", E.float a ]


{-| Text with a dark drop shadow for readability.
-}
label : String -> Float -> Float -> String -> Align -> Int -> List Draw
label s x y c align scale =
    [ Text s (x + toFloat scale) (y + toFloat scale) "#101018" align scale, Text s x y c align scale ]



-- BACKGROUND -------------------------------------------------------------------


background : Theme -> Int -> Float -> Float -> List Draw
background theme seed camY time =
    let
        bands =
            List.length theme.sky

        bh =
            gameH / toFloat bands

        sky =
            List.indexedMap
                (\i c ->
                    let
                        y =
                            toFloat i * bh
                    in
                    Rect 0 y gameW (bh + 1) c
                        :: (case listGet (i + 1) theme.sky of
                                Just next ->
                                    -- retro dither stripes between bands
                                    [ Rect 0 (y + bh - 6) gameW 2 next, Rect 0 (y + bh - 2) gameW 1 next ]

                                Nothing ->
                                    []
                           )
                )
                theme.sky
                |> List.concat

        stars =
            if theme.stars then
                List.map
                    (\i ->
                        let
                            twinkle =
                                sin (time * 2 + toFloat i) > -0.3
                        in
                        if twinkle then
                            Rect (rnd (i * 3) * gameW) (rnd (i * 3 + 1) * gameH * 0.6) 1 1 "#fcfcfc"

                        else
                            Rect 0 0 0 0 "#000"
                    )
                    (List.range 0 40)

            else
                []

        climbed =
            -(camY + 210)

        -- pastel art-deco hotels (the beach level)
        hotels parallax base amp s =
            List.concatMap
                (\i ->
                    let
                        x =
                            toFloat i * 28

                        h =
                            toFloat (round (amp * (0.35 + 0.65 * rnd (s + i))))

                        top =
                            toFloat (round (base + climbed * parallax - h))

                        color =
                            Maybe.withDefault "#f8b8d8" (listGet (modBy 4 (i + s)) [ "#f8b8d8", "#a4e4fc", "#fce4a0", "#c4f0c8" ])

                        windows j =
                            List.map (\k -> Rect (x + 3 + toFloat k * 5) (top + 5 + toFloat j * 6) 3 2 "#fcfcfc") (List.range 0 3)
                    in
                    Rect x top 22 (gameH - top + 1) color
                        :: Rect (x + 6) (top - 3) 10 3 color
                        :: List.concatMap windows (List.range 0 (floor (h / 6) - 2))
                )
                (List.range 0 (ceiling (gameW / 28)))

        -- the ocean with rolling wave highlights
        ocean base =
            let
                top =
                    toFloat (round (base + climbed * 0.05))
            in
            Rect 0 top gameW (gameH - top + 1) "#1c8cd8"
                :: List.map (\i -> Rect (modFloat (time * 10 + toFloat i * 47) gameW) (top + 3 + toFloat (modBy 5 i) * 5) 8 1 "#a4e4fc") (List.range 0 11)

        -- palm tree silhouettes along the shore
        palmRow color base s =
            List.concatMap
                (\i ->
                    let
                        x =
                            toFloat i * 64 + rnd (s + i) * 30

                        foot =
                            toFloat (round (base + climbed * 0.08))

                        trunk j =
                            Rect (x + toFloat (j // 3)) (foot - toFloat j * 4 - 4) 3 4 color

                        crown =
                            foot - 40
                    in
                    Rect 0 foot gameW (gameH - foot + 1) color
                        :: List.map trunk (List.range 0 9)
                        ++ [ Rect (x - 8) crown 22 3 color
                           , Rect (x - 12) (crown + 3) 6 2 color
                           , Rect (x + 10) (crown + 3) 6 2 color
                           , Rect (x - 3) (crown - 3) 12 3 color
                           , Rect (x - 14) (crown + 5) 3 2 color
                           , Rect (x + 15) (crown + 5) 3 2 color
                           ]
                )
                (List.range 0 6)

        -- server towers with blinking LEDs (the circuit level)
        towers color parallax base amp s =
            List.concatMap
                (\i ->
                    let
                        x =
                            toFloat i * 14

                        h =
                            toFloat (round (amp * (0.3 + 0.7 * rnd (s + i))))

                        top =
                            toFloat (round (base + climbed * parallax - h))

                        led j =
                            if rnd (s + i * 7 + j) < 0.45 then
                                [ Rect (x + 3 + toFloat (modBy 2 j) * 5)
                                    (top + 4 + toFloat j * 6)
                                    2
                                    1
                                    (if sin (time * (2 + rnd (s + j)) + toFloat (i + j)) > 0 then
                                        "#58d854"

                                     else
                                        "#d82800"
                                    )
                                ]

                            else
                                []
                    in
                    Rect x top 12 (gameH - top + 1) color
                        :: Rect x top 12 1 "#1c4c6c"
                        :: List.concatMap led (List.range 0 (floor (h / 6) - 1))
                )
                (List.range 0 (ceiling (gameW / 14)))

        -- jagged mountain range: the max of a few triangular peaks, drawn in 2px columns
        hills color cap parallax base amp peaks s =
            let
                peakList =
                    List.map (\k -> ( rnd (s + k * 3) * (gameW + 80) - 40, amp * (0.5 + 0.5 * rnd (s + k * 3 + 1)), 0.6 + 0.8 * rnd (s + k * 3 + 2) )) (List.range 0 (peaks - 1))

                heightAt x =
                    List.foldl (\( px, ph, slope ) acc -> max acc (ph - abs (x - px) * slope)) 6 peakList
                        + toFloat (modBy 3 (round (x * 0.37) + s))
            in
            List.concatMap
                (\i ->
                    let
                        x =
                            toFloat i * 2

                        h =
                            heightAt x

                        top =
                            toFloat (round (base + climbed * parallax - h))
                    in
                    Rect x top 2 (gameH - top + 1) color
                        :: (if h > amp * 0.75 then
                                [ Rect x top 2 (min 4 (h - amp * 0.75)) cap ]

                            else
                                []
                           )
                )
                (List.range 0 (ceiling (gameW / 2)))
    in
    sky
        ++ stars
        ++ (if theme.deco == Circuit then
                towers theme.far 0.03 175 80 (seed * 7) ++ towers theme.near 0.08 220 46 (seed * 13 + 5)

            else if theme.deco == Palms then
                hotels 0.03 170 70 (seed * 7) ++ ocean 178 ++ palmRow theme.near 222 (seed * 13 + 5)

            else
                hills theme.far "#fcfcfc" 0.03 170 90 6 (seed * 7) ++ hills theme.near theme.near 0.08 215 50 8 (seed * 13 + 5)
           )


weather : WeatherFx -> Float -> List Draw
weather fx time =
    let
        flakes color count speed drift size up =
            List.map
                (\i ->
                    let
                        s =
                            i * 7

                        x =
                            modFloat (rnd s * gameW + time * drift * (rnd (s + 1) - 0.5) + sin (time + toFloat i) * 4) gameW

                        y0 =
                            rnd (s + 2) * gameH + time * speed * (0.5 + rnd (s + 3))

                        y =
                            if up then
                                gameH - modFloat y0 gameH

                            else
                                modFloat y0 gameH
                    in
                    Rect x y size size color
                )
                (List.range 0 (count - 1))
    in
    case fx of
        NoFx ->
            []

        Snow ->
            flakes "#fcfcfc" 50 24 30 1 False

        Ember ->
            flakes "#fc7c00" 30 20 20 1 True

        Dust ->
            flakes "#9c6cfc" 20 6 10 1 False

        Sparkle ->
            flakes "#fcd000" 18 10 6 1 True

        Electric ->
            electric time

        Seagulls ->
            List.concatMap
                (\i ->
                    let
                        s =
                            i * 13

                        x =
                            modFloat (rnd (s + 1) * gameW + time * (20 + 15 * rnd s)) (gameW + 30) - 15

                        y =
                            toFloat (round (30 + rnd (s + 2) * 80 + sin (time * 2 + toFloat i) * 4))

                        up =
                            modBy 2 (floor (time * 4 + toFloat i)) == 0

                        ( wingY, bodyY ) =
                            if up then
                                ( y, y + 1 )

                            else
                                ( y + 1, y )
                    in
                    [ Rect x wingY 2 1 "#fcfcfc", Rect (x + 2) bodyY 1 1 "#fcfcfc", Rect (x + 3) wingY 2 1 "#fcfcfc" ]
                )
                (List.range 0 5)

        Sandstorm ->
            -- blowing sand streaks
            List.map
                (\i ->
                    let
                        s =
                            i * 11

                        x =
                            modFloat (rnd s * gameW + time * (90 + 60 * rnd (s + 1))) (gameW + 20) - 10

                        y =
                            rnd (s + 2) * gameH + sin (time * 2 + toFloat i) * 3
                    in
                    Rect x
                        y
                        (toFloat (2 + modBy 3 i))
                        1
                        (if modBy 2 i == 0 then
                            "#f8d878"

                         else
                            "#e0a050"
                        )
                )
                (List.range 0 44)


modFloat : Float -> Float -> Float
modFloat a m =
    a - m * toFloat (floor (a / m))



-- WORLD ------------------------------------------------------------------------


world : Bool -> World -> List Draw
world paused w =
    let
        shakeX =
            (rnd w.tick - 0.5) * w.shake

        shakeY =
            (rnd (w.tick + 1) - 0.5) * w.shake

        sx x =
            x + shakeX

        sy y =
            y - w.camY + shakeY

        visible y h =
            y + h > w.camY - 20 && y < w.camY + gameH + 20
    in
    background w.level.theme w.level.n w.camY w.time
        ++ weather w.level.theme.fx w.time
        ++ List.concatMap (platform w sx sy) (List.filter (\p -> visible p.y p.h) w.level.platforms)
        ++ safetyNet w sy
        ++ List.concatMap (zone sx sy) w.zones
        ++ List.concatMap (item w.time sx sy) (List.filter (\i -> visible i.y 8) w.items)
        ++ List.concatMap (enemy sx sy) (List.filter (\e -> e.active && not e.dead) w.enemies)
        ++ (case w.boss of
                Just b ->
                    if visible b.y b.h then
                        boss w sx sy b

                    else
                        []

                Nothing ->
                    []
           )
        ++ List.concatMap (player w sx sy) w.players
        ++ List.concatMap (projectile w sx sy) w.projs
        ++ List.map (\b -> Line (sx b.x1) (sy b.y1) (sx b.x2) (sy b.y2) 2 b.color) w.beams
        ++ List.concatMap (particleDraw sx sy) (List.reverse w.particles)
        ++ List.concatMap (\wr -> [ Spr "warn" wr.x 22 TopLeft False (modBy 8 w.tick < 4) 1 ]) w.warnings
        ++ hud w
        ++ (if paused then
                []

            else
                bannerDraw w.banner
           )
        ++ (if paused then
                [ Alpha 0.6, Rect 0 0 gameW gameH "#101018", Alpha 1 ]
                    ++ label "PAUSED" (gameW / 2) 100 "#fcfcfc" Mid 2
                    ++ label "PRESS ~ (OR P) TO KEEP CLIMBING" (gameW / 2) 124 "#bcbcc4" Mid 1
                    ++ label "M = MUTE" (gameW / 2) 136 "#bcbcc4" Mid 1

            else
                []
           )


platform : World -> (Float -> Float) -> (Float -> Float) -> Platform -> List Draw
platform w sx sy p =
    let
        th =
            w.level.theme

        jitter =
            if p.ct >= 0 then
                (rnd (w.tick + p.id) - 0.5) * 2

            else
                0

        x =
            sx p.x + jitter

        y =
            sy p.y

        top =
            if p.ice then
                "#c4f0fc"

            else
                th.top
    in
    if p.gone then
        []

    else
        case p.kind of
            Normal ->
                [ Rect x y p.w p.h th.body
                , Rect x y p.w 2 top
                , Rect x (y + p.h - 1) p.w 1 th.edge
                , Rect x y 1 p.h th.edge
                , Rect (x + p.w - 1) y 1 p.h th.edge
                ]
                    ++ deco th.deco x y p.w p.id
                    ++ (if p.crumble then
                            [ Line (x + p.w * 0.3) (y + 2) (x + p.w * 0.4) (y + 5) 1 th.edge, Line (x + p.w * 0.7) (y + 2) (x + p.w * 0.6) (y + 5) 1 th.edge ]

                        else
                            []
                       )
                    ++ (case p.moving of
                            Just _ ->
                                [ Rect (x + 2) (y + 3) 2 1 "#fcd000", Rect (x + p.w - 4) (y + 3) 2 1 "#fcd000" ]

                            Nothing ->
                                []
                       )

            Ground ->
                [ Rect x y p.w (p.h + 20) th.body, Rect x y p.w 3 top, Rect x (y + 3) p.w 1 th.edge ]
                    ++ List.map (\i -> Rect (x + toFloat i * 24 + 6) (y + 10 + toFloat (modBy 3 i) * 5) 4 2 th.edge) (List.range 0 15)

            Arena ->
                [ Rect x y p.w (p.h + 4) th.edge
                , Rect x y p.w 4 top
                , Rect x (y + 4) p.w 1 "#101018"
                ]
                    ++ List.concatMap
                        (\i ->
                            [ Rect (x + toFloat i * 16 + toFloat (modBy 2 (i // 24)) * 8) (y + 8) 14 6 th.body
                            , Rect (x + toFloat i * 16 + 8) (y + 16) 14 6 th.body
                            ]
                        )
                        (List.range 0 24)
                    ++ [ Rect (x + 8) (y - 22) 2 22 "#7c7c84", Rect (x + 10) (y - 22) 10 7 "#d82800", Rect (x + p.w - 10) (y - 22) 2 22 "#7c7c84", Rect (x + p.w - 20) (y - 22) 10 7 "#d82800" ]


deco : Deco -> Float -> Float -> Float -> Int -> List Draw
deco d x y w id =
    let
        spots color h =
            List.filterMap
                (\i ->
                    let
                        px =
                            x + 2 + rnd (id * 11 + i) * (w - 4)
                    in
                    if rnd (id * 5 + i) < 0.6 then
                        Just (Rect px (y - h) 1 h color)

                    else
                        Nothing
                )
                (List.range 0 (round (w / 8)))
    in
    case d of
        Grass ->
            spots "#58d854" 2 ++ spots "#00a800" 1

        SnowCap ->
            [ Rect (x - 1) (y - 1) (w + 2) 2 "#fcfcfc" ]

        Lava ->
            spots "#fc7c00" 1 ++ [ Rect (x + 2) (y + 3) (w - 4) 1 "#d82800" ]

        GoldTrim ->
            [ Rect x (y + 3) w 1 "#fcfc58" ]

        Crystal ->
            spots "#00e8d8" 2

        Stone ->
            spots "#7c7c84" 1

        Circuit ->
            -- gold pins under the chip, a pin-1 dot and a status LED
            Rect (x + 2) (y + 3) 1 1 "#fcfcfc"
                :: Rect (x + w - 4)
                    (y + 3)
                    2
                    1
                    (if modBy 2 id == 0 then
                        "#58d854"

                     else
                        "#d82800"
                    )
                :: List.map (\i -> Rect (x + 2 + toFloat i * 4) (y + 6) 2 2 "#d8b030") (List.range 0 (floor ((w - 4) / 4)))

        Palms ->
            -- boardwalk planks, plus a palm tree or a beach umbrella
            List.map (\i -> Rect (x + 6 + toFloat i * 8) (y + 2) 1 3 "#8c5c2c") (List.range 0 (floor ((w - 8) / 8)))
                ++ (let
                        px =
                            toFloat (round (x + 6 + rnd (id * 13) * (w - 14)))

                        r =
                            rnd (id * 7)
                    in
                    if r < 0.35 then
                        [ Rect px (y - 6) 2 6 "#a0582c"
                        , Rect (px + 1) (y - 12) 2 6 "#a0582c"
                        , Rect (px - 3) (y - 14) 10 2 "#3cb04c"
                        , Rect (px - 5) (y - 12) 3 2 "#3cb04c"
                        , Rect (px + 7) (y - 12) 3 2 "#3cb04c"
                        , Rect px (y - 12) 1 1 "#6c4420"
                        , Rect (px + 3) (y - 12) 1 1 "#6c4420"
                        ]

                    else if r < 0.6 then
                        [ Rect px (y - 10) 1 10 "#fcfcfc"
                        , Rect (px - 5) (y - 12) 11 2 "#fc3c3c"
                        , Rect (px - 3) (y - 13) 7 1 "#fc3c3c"
                        , Rect (px - 1) (y - 13) 3 3 "#fcfcfc"
                        ]

                    else
                        []
                   )

        Cactus ->
            Rect x (y + 3) w 1 "#c08840"
                :: (if rnd (id * 7) < 0.5 then
                        let
                            px =
                                toFloat (round (x + 4 + rnd (id * 13) * (w - 10)))
                        in
                        [ Rect px (y - 7) 2 7 "#3c9c3c"
                        , Rect px (y - 7) 1 7 "#58d854"
                        , Rect (px - 2) (y - 5) 1 3 "#3c9c3c"
                        , Rect (px - 2) (y - 3) 2 1 "#3c9c3c"
                        , Rect (px + 3) (y - 6) 1 3 "#3c9c3c"
                        , Rect (px + 2) (y - 4) 2 1 "#3c9c3c"
                        ]

                    else
                        []
                   )


zone : (Float -> Float) -> (Float -> Float) -> Zone -> List Draw
zone sx sy z =
    [ Alpha (0.25 * min 1 (z.life / 0.5))
    , Circ (sx z.x)
        (sy z.y)
        z.r
        (if z.hostile then
            "#00a800"

         else
            "#b8f818"
        )
        True
    , Alpha 1
    ]


item : Float -> (Float -> Float) -> (Float -> Float) -> Item -> List Draw
item time sx sy it =
    let
        bob =
            if it.phys then
                0

            else
                toFloat (round (sin (it.t * 3) * 1.5))

        x =
            sx it.x

        y =
            sy it.y + bob

        blink =
            it.life > 0 && it.t > it.life - 2 && modBy 2 (floor (it.t * 10)) == 0
    in
    if blink then
        []

    else
        case it.kind of
            Coin ->
                [ Spr
                    (if modBy 4 (floor (it.t * 8)) == 1 then
                        "i_coin_1"

                     else
                        "i_coin_0"
                    )
                    x
                    y
                    TopLeft
                    False
                    False
                    1
                ]

            Diamond ->
                [ Spr "i_diamond" x y TopLeft False (modBy 20 (floor (it.t * 10)) == 0) 1 ]

            Heart ->
                [ Spr "i_heart" x y TopLeft False False 1 ]

            Cash ->
                [ Spr "i_cash" x y TopLeft False False 1 ]

            WeaponItem wp ->
                let
                    glow =
                        sin (time * 6) > 0
                in
                [ Alpha
                    (if glow then
                        0.5

                     else
                        0.25
                    )
                , Circ (x + 6) (y + 3) 10 "#fcfc58" False
                , Circ (x + 6) (y + 3) 9 "#fcfc58" False
                , Alpha 1
                , Spr (Data.weaponDef wp).sprite (x + 6) (y + 3) Center False False 1
                ]


enemy : (Float -> Float) -> (Float -> Float) -> Enemy -> List Draw
enemy sx sy e =
    let
        frame =
            modBy 2 (floor (e.t * 6))

        id =
            case e.kind of
                Goon ->
                    "e_goon_"

                BabyDragon ->
                    "e_baby_"

                Archer ->
                    "e_archer_"

                Bodyguard ->
                    "e_guard_"

        cx =
            sx (e.x + e.w / 2)

        bottom =
            sy (e.y + e.h)
    in
    Spr (id ++ String.fromInt frame) cx (bottom + 1) BottomCenter (e.facing < 0) (e.flash > 0) 1
        :: (if e.hp < e.maxHp then
                [ Rect (cx - 6) (sy e.y - 5) 12 2 "#101018", Rect (cx - 6) (sy e.y - 5) (12 * max 0 e.hp / e.maxHp) 2 "#fc3c3c" ]

            else
                []
           )


boss : World -> (Float -> Float) -> (Float -> Float) -> Boss -> List Draw
boss w sx sy b =
    let
        attacking =
            b.tele > 0 || b.breath /= Nothing || not (List.isEmpty b.beams) || (b.atk /= Nothing && b.vx /= 0)

        frame =
            if b.state == Defeated then
                2

            else if attacking then
                2

            else if b.state == Sleeping then
                0

            else
                modBy 2 (floor (b.t * 4))

        cx =
            sx (b.x + b.w / 2)

        bottom =
            sy (b.y + b.h)

        sprite =
            if b.state == Defeated then
                []

            else
                [ Spr (b.def.sprite ++ "_" ++ String.fromInt frame) cx (bottom + 1) BottomCenter (b.facing < 0) (b.flash > 0 || (b.tele > 0 && modBy 6 w.tick < 2)) 1 ]

        zzz =
            if b.state == Sleeping then
                label (String.left (1 + modBy 3 (floor (w.time * 2))) "ZZZ") (cx + b.w / 2) (sy b.y - 6) "#fcfcfc" Left 1

            else
                []

        beamDraws =
            List.concatMap
                (\bm ->
                    let
                        ( ex, ey ) =
                            Boss.eyePoint b bm.eye
                    in
                    if bm.on then
                        [ Line (sx ex) (sy ey) (sx bm.px) (sy b.floorY) 4 "#d82800", Line (sx ex) (sy ey) (sx bm.px) (sy b.floorY) 2 "#fc3c3c", Line (sx ex) (sy ey) (sx bm.px) (sy b.floorY) 1 "#fce4a0" ]

                    else
                        [ Alpha 0.5, Line (sx ex) (sy ey) (sx bm.px) (sy b.floorY) 1 "#fc3c3c", Alpha 1 ]
                )
                b.beams

        quote =
            if b.quoteT > 0 && b.state == Fighting && b.quote /= "" then
                let
                    tw =
                        toFloat (String.length b.quote * 6 + 6)

                    qx =
                        clamp 2 (gameW - tw - 2) (cx - tw / 2)

                    qy =
                        max 24 (sy b.y - 18)
                in
                [ Rect qx qy tw 11 "#fcfcfc", Rect (cx - 1) (qy + 11) 3 3 "#fcfcfc", Text b.quote (qx + 3) (qy + 2) "#101018" Left 1 ]

            else
                []

        jet =
            case b.atk of
                Just a ->
                    case a.spec of
                        Jet _ ->
                            if a.stage == 1 then
                                [ Spr "x_jet" (sx a.tx) 30 Center (a.ty < 0) False 1 ]

                            else
                                []

                        _ ->
                            []

                Nothing ->
                    []
    in
    sprite ++ beamDraws ++ zzz ++ quote ++ jet


player : World -> (Float -> Float) -> (Float -> Float) -> Player -> List Draw
player w sx sy p =
    let
        prefix =
            "p" ++ String.fromInt p.idx ++ "_"

        cx =
            sx (p.x + 5)

        bottom =
            sy (p.y + 15)

        def =
            Data.playerDef p.idx
    in
    if p.hp <= 0 then
        [ Alpha 0.55, Spr (prefix ++ "ghost") cx bottom BottomCenter (p.facing < 0) False 1, Alpha 1 ]
            ++ (if List.any (\q -> q.hp > 0) w.players then
                    label (String.fromInt (ceiling (World.reviveTime - p.ghostT))) cx (bottom - 26) "#fcfcfc" Mid 1

                else
                    []
               )

    else if p.invuln > 0 && modBy 4 w.tick < 2 then
        []

    else
        let
            pose =
                if not p.onGround then
                    "jump"

                else if abs p.vx > 10 then
                    if modBy 2 (floor (p.anim * 8)) == 0 then
                        "walk1"

                    else
                        "walk2"

                else
                    "idle"

            flip =
                p.facing < 0

            atk =
                p.atkT > 0

            wd =
                Data.weaponDef p.weapon

            ( wsprite, fwd ) =
                case p.weapon of
                    Fists ->
                        if atk then
                            ( "w_fists_atk", 7 )

                        else
                            ( "w_fists", 4 )

                    Nunchucks ->
                        if atk then
                            ( "w_nunchucks_atk", 5 )

                        else
                            ( "w_nunchucks", 4 )

                    Sword ->
                        if atk then
                            ( "w_sword_atk", 7 )

                        else
                            ( "w_sword", 7 )

                    _ ->
                        ( wd.sprite
                        , if atk then
                            4

                          else
                            5
                        )

            weapon =
                Spr wsprite (cx + p.facing * fwd) (bottom - 5) Center flip False 1

            slash =
                if atk && (p.weapon == Sword || p.weapon == Nunchucks || p.weapon == Fists) then
                    let
                        r =
                            case p.weapon of
                                Sword ->
                                    16

                                Nunchucks ->
                                    13

                                _ ->
                                    8
                    in
                    [ Alpha 0.6, Circ (cx + p.facing * (r * 0.6)) (bottom - 8) r "#fcfcfc" False, Alpha 1 ]

                else
                    []

            hpBar =
                [ Rect (cx - 6) (bottom - 21) 12 3 "#101018"
                , Rect (cx - 5) (bottom - 20) (10 * p.hp / 100) 1 (hpColor p.hp)
                , Rect (cx - 2) (bottom - 26) 5 1 def.color
                , Rect (cx - 1) (bottom - 25) 3 1 def.color
                , Rect cx (bottom - 24) 1 1 def.color
                ]

            prompt =
                if p.prompt > 0 then
                    label ("PRESS " ++ def.atk) cx (bottom - 34) "#fcfcfc" Mid 1

                else
                    []

            frozen =
                if p.slow > 0 then
                    [ Alpha 0.4, Rect (cx - 6) (bottom - 16) 12 16 "#3cbcfc", Alpha 1 ]

                else
                    []
        in
        slash ++ [ Spr (Data.spriteFor p) cx (bottom + 1) BottomCenter flip (p.hurtFx > 0) 1, weapon ] ++ frozen ++ hpBar ++ prompt


hpColor : Float -> String
hpColor hp =
    if hp > 60 then
        "#58d854"

    else if hp > 30 then
        "#fcd000"

    else
        "#fc3c3c"


projectile : World -> (Float -> Float) -> (Float -> Float) -> Proj -> List Draw
projectile w sx sy p =
    let
        x =
            sx p.x

        y =
            sy p.y

        flip =
            p.vx < 0

        spr id =
            [ Spr id x y Center flip False 1 ]

        shadow =
            if p.shadow then
                case w.boss of
                    Just b ->
                        [ Alpha 0.4, Rect (x - 4) (sy b.floorY - 2) 8 2 "#101018", Alpha 1 ]

                    Nothing ->
                        []

            else
                []
    in
    shadow
        ++ (case p.kind of
                Bullet ->
                    [ Rect (x - 1) (y - 1) 3 2 "#fcfc58" ]

                Rocket ->
                    spr "x_rocket"

                FartP ->
                    spr "x_fart"

                Magic ->
                    spr "x_magic"

                Fireball ->
                    spr "x_fireball"

                Spear ->
                    spr "x_spear"

                Rock ->
                    spr "x_rock"

                Sonic ->
                    [ Circ x y p.r "#9c6cfc" False, Circ x y (p.r - 2) "#f878f8" False ]

                MoneyBag ->
                    spr "x_moneybag"

                CoinP ->
                    spr "i_coin_0"

                Snowball ->
                    spr "x_snowball"

                Icicle ->
                    spr "x_icicle"

                Arrow ->
                    spr "x_arrow"

                Bone ->
                    [ Spr "x_bone" x y Center (modBy 8 w.tick < 4) False 1 ]

                DiamondP ->
                    spr "i_diamond"

                BitcoinP ->
                    spr "x_btc"

                Wave ->
                    spr "x_wave"

                Spark ->
                    spr "x_spark"

                Crate ->
                    spr "x_crate"
           )


particleDraw : (Float -> Float) -> (Float -> Float) -> Particle -> List Draw
particleDraw sx sy p =
    let
        k =
            p.life / p.maxLife
    in
    case p.kind of
        Dot ->
            [ Rect (sx p.x) (sy p.y) (max 1 p.size) (max 1 p.size) p.color ]

        Puff ->
            [ Alpha (min 1 (k * 1.2)), Circ (sx p.x) (sy p.y) (p.size * (1.4 - k * 0.6)) p.color True, Alpha 1 ]

        Ring ->
            [ Circ (sx p.x) (sy p.y) (p.size * (1 - k * 0.7)) p.color False ]

        Txt s ->
            label s (sx p.x) (sy p.y) p.color Mid 1

        Img id flip ->
            [ Alpha (min 1 (k * 2.5)), Spr id (sx p.x) (sy p.y) Center flip False 1, Alpha 1 ]



-- HUD --------------------------------------------------------------------------


hud : World -> List Draw
hud w =
    let
        panel p =
            let
                def =
                    Data.playerDef p.idx

                x =
                    toFloat p.idx * 96 + 2
            in
            [ Rect x 2 92 17 "#101018"
            , Rect x 2 2 17 def.color
            , Text def.name (x + 4) 3 def.color Left 1
            ]
                ++ (if p.hp > 0 then
                        [ Rect (x + 17) 3 42 6 "#404048"
                        , Rect (x + 18) 4 (40 * p.hp / 100) 4 (hpColor p.hp)
                        , Text (String.fromInt (ceiling p.hp)) (x + 62) 3 "#fcfcfc" Left 1
                        ]

                    else
                        [ Text "KO" (x + 18) 3 "#fc3c3c" Left 1 ]
                   )
                ++ [ Spr (Data.weaponDef p.weapon).sprite (x + 10) 14 Center False False 1
                   , Spr "i_coin_0" (x + 22) 10 TopLeft False False 1
                   , Text (String.fromInt p.gold) (x + 31) 11 "#fcd000" Left 1
                   , Spr "i_diamond" (x + 56) 10 TopLeft False False 1
                   , Text (String.fromInt p.diamonds) (x + 66) 11 "#00e8d8" Left 1
                   ]

        lvl =
            w.level

        progress =
            clamp 0 1 ((-210 - w.camY) / (-210 - lvl.arenaCamTop))

        barTop =
            34

        barH =
            150

        dot p =
            let
                k =
                    clamp 0 1 (-(p.y + 15) / lvl.height)
            in
            Rect (gameW - 9) (barTop + barH - k * barH - 1) 4 2 (Data.playerDef p.idx).color

        progressBar =
            case w.phase of
                Climb ->
                    [ Rect (gameW - 8) barTop 2 barH "#101018"
                    , Rect (gameW - 8) (barTop + barH * (1 - progress)) 2 (barH * progress) "#fcd000"
                    , Spr
                        (if lvl.n == 10 then
                            "i_crown"

                         else
                            "i_skull"
                        )
                        (gameW - 7)
                        (barTop - 2)
                        BottomCenter
                        False
                        False
                        1
                    ]
                        ++ List.map dot (List.filter (\p -> p.hp > 0) w.players)
                        ++ label ("LV" ++ String.fromInt lvl.n ++ " " ++ lvl.name) 4 230 "#fcfcfc" Left 1

                _ ->
                    []

        bossBar =
            case ( w.phase, w.boss ) of
                ( BossFight, Just b ) ->
                    [ Rect 92 226 200 9 "#101018"
                    , Rect 93
                        227
                        (198 * max 0 b.hp / b.maxHp)
                        7
                        (if b.enraged then
                            "#fc3c3c"

                         else
                            "#d82800"
                        )
                    , Rect 93 227 (198 * max 0 b.hp / b.maxHp) 2 "#fc7460"
                    ]
                        ++ label b.def.name (gameW / 2) 217 "#fcfcfc" Mid 1

                _ ->
                    []
    in
    List.concatMap panel w.players ++ progressBar ++ bossBar


bannerDraw : Maybe Banner -> List Draw
bannerDraw banner =
    case banner of
        Just b ->
            let
                a =
                    min 1 (b.t * 3)
            in
            [ Alpha (0.5 * a), Rect 0 74 gameW 44 "#101018", Alpha a ]
                ++ label b.text (gameW / 2) 82 b.color Mid 2
                ++ label b.sub (gameW / 2) 104 "#fcfcfc" Mid 1
                ++ [ Alpha 1 ]

        Nothing ->
            []



-- SCREENS ----------------------------------------------------------------------


summitTheme : Theme
summitTheme =
    (Data.levelInfo 11).theme


title : Float -> List Bool -> Int -> List Draw
title time joined startLevel =
    let
        slot i on =
            let
                def =
                    Data.playerDef i

                x =
                    toFloat i * 92 + 18

                bob =
                    if on then
                        toFloat (round (sin (time * 6 + toFloat i) * 2))

                    else
                        0
            in
            [ Rect x 136 82 72 "#101018"
            , Rect x 136 82 2 def.color
            , Text def.name (x + 41) 141 def.color Mid 1
            ]
                ++ (if on then
                        [ Spr ("p" ++ String.fromInt i ++ "_idle") (x + 41) (182 + bob) BottomCenter False False 2
                        , Text "READY!" (x + 41) 188 "#58d854" Mid 1
                        ]

                    else
                        [ Alpha (0.4 + 0.3 * sin (time * 4)), Spr ("p" ++ String.fromInt i ++ "_ghost") (x + 41) 182 BottomCenter False False 2, Alpha 1, Text ("PRESS " ++ def.jump) (x + 41) 188 "#bcbcc4" Mid 1 ]
                   )
                ++ [ Text def.keys (x + 41) 198 "#7c7c84" Mid 1 ]

        anyJoined =
            List.any identity joined
    in
    background summitTheme 11 -210 time
        ++ weather Ember time
        ++ [ Spr "dragon_title" (gameW / 2 + 90) (126 + sin (time * 1.5) * 4) BottomCenter True False 1 ]
        ++ label "MOUNT" 24 26 "#fcd000" Left 3
        ++ label "DIABLO" 24 50 "#fc3c3c" Left 4
        ++ label "A 4-PLAYER CLIMBING ADVENTURE" 26 86 "#fcfcfc" Left 1
        ++ label "UP = JUMP (TAP TWICE TO DOUBLE JUMP)" 26 100 "#bcbcc4" Left 1
        ++ label "DOWN = ATTACK / GRAB WEAPON" 26 110 "#bcbcc4" Left 1
        ++ label "~ = PAUSE    M = MUTE" 26 120 "#7c7c84" Left 1
        ++ List.concat (List.indexedMap slot joined)
        ++ label
            (if startLevel == 1 then
                "NUMBER KEYS: PICK A STARTING LEVEL"

             else
                "STARTING AT LEVEL " ++ String.fromInt startLevel ++ ": " ++ (Data.levelInfo startLevel).name
            )
            (gameW / 2)
            211
            (if startLevel == 1 then
                "#7c7c84"

             else
                "#00e8d8"
            )
            Mid
            1
        ++ (if anyJoined then
                if sin (time * 5) > -0.2 then
                    label "PRESS ENTER TO START!" (gameW / 2) 226 "#fcd000" Mid 1

                else
                    []

            else
                label "PRESS YOUR JUMP KEY TO JOIN" (gameW / 2) 226 "#fcfcfc" Mid 1
           )


story : Float -> List Draw
story time =
    let
        lines =
            [ ( "THE EVIL KING IS GREEDY, OLD, FAT AND", "#fcfcfc" )
            , ( "VERY, VERY ORANGE.", "#fc9838" )
            , ( "HE STOLE ALL THE GOLD, DIAMONDS AND", "#fcfcfc" )
            , ( "BITCOIN IN THE LAND - AND WON'T SHARE!", "#fcfcfc" )
            , ( "", "#fff" )
            , ( "HIS TREASURE IS HIDDEN ON TOP OF", "#fcfcfc" )
            , ( "MOUNT DIABLO, GUARDED BY A DRAGON.", "#fc3c3c" )
            , ( "", "#fff" )
            , ( "CLIMB 10 LEVELS TOGETHER. BEAT THE", "#fcfcfc" )
            , ( "BOSSES. GRAB WEAPONS. TAKE IT BACK!", "#fcd000" )
            ]
    in
    background (Data.levelInfo 10).theme 10 -210 time
        ++ [ Alpha 0.7, Rect 8 8 (gameW - 16) (gameH - 16) "#101018", Alpha 1 ]
        ++ [ Spr "king_portrait" 330 (110 + sin (time * 3) * 2) BottomCenter True False 1 ]
        ++ List.concat (List.indexedMap (\i ( s, c ) -> label s 20 (22 + toFloat i * 13) c Left 1) lines)
        ++ (if sin (time * 5) > -0.2 then
                label "PRESS ENTER" (gameW / 2) 214 "#fcd000" Mid 1

            else
                []
           )


playerStats : List Player -> Float -> List Draw
playerStats players y =
    List.concat
        (List.indexedMap
            (\i p ->
                let
                    def =
                        Data.playerDef p.idx

                    x =
                        toFloat i * 92 + 18
                in
                [ Rect x y 84 46 "#101018"
                , Rect x y 84 2 def.color
                , Spr ("c_" ++ (Data.characterInfo p.character).key ++ "_idle") (x + 12) (y + 26) BottomCenter False False 1
                , Text def.name (x + 24) (y + 6) def.color Left 1
                , Spr "i_coin_0" (x + 24) (y + 16) TopLeft False False 1
                , Text (String.fromInt p.gold) (x + 34) (y + 17) "#fcd000" Left 1
                , Spr "i_diamond" (x + 52) (y + 16) TopLeft False False 1
                , Text (String.fromInt p.diamonds) (x + 62) (y + 17) "#00e8d8" Left 1
                , Text ("KOS " ++ String.fromInt p.kills) (x + 24) (y + 28) "#bcbcc4" Left 1
                , Spr (Data.weaponDef p.weapon).sprite (x + 12) (y + 36) Center False False 1
                ]
            )
            players
        )


interlude : Float -> Int -> String -> List Player -> List Draw
interlude time nextN taunt players =
    let
        info =
            Data.levelInfo nextN

        tw =
            toFloat (String.length taunt * 6 + 8)

        bubbleX =
            clamp 4 (gameW - tw - 4) (gameW / 2 - tw / 2)
    in
    background info.theme nextN -210 time
        ++ [ Alpha 0.6, Rect 0 0 gameW gameH "#101018", Alpha 1 ]
        ++ label ("LEVEL " ++ String.fromInt (nextN - 1) ++ " CLEAR!") (gameW / 2) 10 "#fcd000" Mid 2
        ++ [ Spr "king_portrait" (gameW / 2) (100 + sin (time * 4) * 2) BottomCenter False False 1
           , Rect bubbleX 102 tw 13 "#fcfcfc"
           , Text taunt (bubbleX + 4) 105 "#101018" Left 1
           ]
        ++ label "THE EVIL KING SAYS:" (gameW / 2) 32 "#fc9838" Mid 1
        ++ playerStats players 122
        ++ label
            (if nextN == 11 then
                "NEXT: THE SUMMIT - DIABLO THE DRAGON!"

             else
                "NEXT: LEVEL " ++ String.fromInt nextN ++ " - " ++ info.name
            )
            (gameW / 2)
            178
            "#fcfcfc"
            Mid
            1
        ++ (if sin (time * 5) > -0.2 then
                label "PRESS ENTER TO KEEP CLIMBING" (gameW / 2) 214 "#fcd000" Mid 1

            else
                []
           )


gameOver : Float -> World -> List Draw
gameOver time w =
    world False w
        ++ [ Alpha 0.75, Rect 0 0 gameW gameH "#101018", Alpha 1 ]
        ++ label "OH NO!" (gameW / 2) 30 "#fc3c3c" Mid 3
        ++ [ Spr "king_portrait" (gameW / 2) (126 + sin (time * 10) * 2) BottomCenter False False 1 ]
        ++ label "HA HA HA! MY GOLD IS SAFE!" (gameW / 2) 136 "#fc9838" Mid 1
        ++ label ("LEVEL " ++ String.fromInt w.level.n ++ " - " ++ w.level.name) (gameW / 2) 160 "#fcfcfc" Mid 1
        ++ (if sin (time * 5) > -0.2 then
                label "PRESS ENTER TO TRY AGAIN" (gameW / 2) 200 "#fcd000" Mid 1

            else
                []
           )


victory : Float -> List Player -> List Draw
victory time players =
    let
        total =
            List.sum (List.map .gold players)

        gems =
            List.sum (List.map .diamonds players)

        fireworks =
            List.concatMap
                (\i ->
                    let
                        period =
                            1.6

                        tt =
                            time + toFloat i * 0.53

                        cycle =
                            floor (tt / period)

                        k =
                            (tt - toFloat cycle * period) / period

                        s =
                            cycle * 31 + i * 7

                        cx =
                            rndRange s 40 (gameW - 40)

                        cy =
                            rndRange (s + 1) 30 110

                        colors =
                            [ "#fc3c3c", "#fcd000", "#00e8d8", "#58d854", "#f878f8" ]

                        c =
                            Maybe.withDefault "#fff" (listGet (modBy 5 (s + i)) colors)
                    in
                    List.map
                        (\j ->
                            let
                                a =
                                    toFloat j / 12 * 2 * pi
                            in
                            Rect (cx + cos a * k * 30) (cy + sin a * k * 30 + k * k * 12) 2 2 c
                        )
                        (List.range 0 11)
                )
                (List.range 0 4)
    in
    background (Data.levelInfo 11).theme 11 -210 time
        ++ fireworks
        ++ label "YOU WIN!" (gameW / 2) 20 "#fcd000" Mid 3
        ++ label "DIABLO IS DEFEATED AND THE KING RAN AWAY!" (gameW / 2) 50 "#fcfcfc" Mid 1
        ++ label "ALL THE GOLD GOES BACK TO EVERYBODY." (gameW / 2) 62 "#fcfcfc" Mid 1
        ++ label "SHARING IS CARING!" (gameW / 2) 78 "#58d854" Mid 2
        ++ label ("TEAM TREASURE: " ++ String.fromInt total ++ " GOLD, " ++ String.fromInt gems ++ " DIAMONDS") (gameW / 2) 104 "#fcd000" Mid 1
        ++ playerStats players 122
        ++ (if sin (time * 5) > -0.2 then
                label "PRESS ENTER TO PLAY AGAIN" (gameW / 2) 214 "#fcd000" Mid 1

            else
                []
           )


{-| Draw a 1px rectangle outline.
-}
outlineBox : Float -> Float -> Float -> Float -> String -> List Draw
outlineBox x y w h c =
    [ Rect x y w 1 c, Rect x (y + h - 1) w 1 c, Rect x y 1 h c, Rect (x + w - 1) y 1 h c ]


characterSelect : Float -> List Bool -> List Int -> List Bool -> List Draw
characterSelect time joined picks ready =
    let
        chars =
            Data.characters

        iconX i =
            toFloat i * 31 + 22

        strip =
            List.concat
                (List.indexedMap
                    (\i c ->
                        [ Rect (iconX i - 13) 30 26 26 "#101018"
                        , Spr ("c_" ++ (Data.characterInfo c).key ++ "_idle") (iconX i) 53 BottomCenter False False 1
                        ]
                    )
                    chars
                )

        cursors =
            List.concat
                (List.indexedMap
                    (\i ( on, pick ) ->
                        if on then
                            let
                                o =
                                    toFloat i
                            in
                            outlineBox (iconX pick - 14 - o) (29 - o) (28 + 2 * o) (28 + 2 * o) (Data.playerDef i).color

                        else
                            []
                    )
                    (List.map2 Tuple.pair joined picks)
                )

        panel i ( ( on, pick ), isReady ) =
            let
                def =
                    Data.playerDef i

                x =
                    toFloat i * 92 + 18

                y =
                    66

                c =
                    Data.characterAt pick

                info =
                    Data.characterInfo c

                pose =
                    if isReady then
                        if modBy 2 (floor (time * 6)) == 0 then
                            "walk1"

                        else
                            "walk2"

                    else
                        "idle"

                hop =
                    if isReady then
                        toFloat (round (abs (sin (time * 6)) * -4))

                    else
                        0

                blink =
                    sin (time * 6) > -0.3
            in
            [ Rect x y 82 140 "#101018", Rect x y 82 2 def.color, Text def.name (x + 41) (y + 5) def.color Mid 1 ]
                ++ (if on then
                        [ Alpha 0.25
                        , Circ (x + 41) (y + 70) 22 def.color True
                        , Alpha 1
                        , Spr ("c_" ++ info.key ++ "_" ++ pose) (x + 41) (y + 82 + hop) BottomCenter False False 3
                        ]
                            ++ (if not isReady && blink then
                                    [ Text "<" (x + 5) (y + 54) "#fcfcfc" Left 1, Text ">" (x + 72) (y + 54) "#fcfcfc" Left 1 ]

                                else
                                    []
                               )
                            ++ label info.name (x + 41) (y + 90) "#fcfcfc" Mid 1
                            ++ [ Text info.blurb (x + 41) (y + 102) "#7c7c84" Mid 1 ]
                            ++ (if isReady then
                                    label "READY!" (x + 41) (y + 116) "#58d854" Mid 1
                                        ++ [ Text (def.jump ++ " = CHANGE") (x + 41) (y + 128) "#7c7c84" Mid 1 ]

                                else
                                    [ Text (def.atk ++ " = PICK") (x + 41) (y + 118) "#fcd000" Mid 1 ]
                               )

                    else
                        [ Text "NOT" (x + 41) (y + 60) "#404048" Mid 1, Text "PLAYING" (x + 41) (y + 70) "#404048" Mid 1 ]
                   )
    in
    background (Data.levelInfo 1).theme 1 -210 time
        ++ [ Alpha 0.5, Rect 0 0 gameW gameH "#101018", Alpha 1 ]
        ++ label "CHOOSE YOUR CHARACTER!" (gameW / 2) 8 "#fcd000" Mid 2
        ++ strip
        ++ cursors
        ++ List.concat (List.indexedMap panel (List.map2 Tuple.pair (List.map2 Tuple.pair joined picks) ready))
        ++ label "LEFT / RIGHT = CHOOSE    DOWN = READY" (gameW / 2) 222 "#fcfcfc" Mid 1


{-| A rope net along the bottom of the screen once the ground is out of view:
players land on it instead of falling off-screen.
-}
safetyNet : World -> (Float -> Float) -> List Draw
safetyNet w sy =
    let
        floorY =
            World.screenFloor w

        y =
            sy floorY
    in
    if floorY >= 0 then
        []

    else
        [ Alpha 0.55 ]
            ++ List.concatMap
                (\i ->
                    let
                        x =
                            toFloat i * 8
                    in
                    [ Rect x y 8 1 "#fce4a0", Line x (y + 1) (x + 4) (y + 4) 1 "#a0582c", Line (x + 4) (y + 4) (x + 8) (y + 1) 1 "#a0582c" ]
                )
                (List.range 0 (ceiling (gameW / 8)))
            ++ [ Alpha 1 ]


{-| Circuit-board traces with electric pulses running along them, plus the
occasional lightning zap across the sky.
-}
electric : Float -> List Draw
electric time =
    let
        trace i =
            let
                s =
                    i * 17

                y =
                    toFloat (round (20 + rnd s * (gameH - 40)))

                x0 =
                    toFloat (round (rnd (s + 1) * gameW * 0.7))

                len =
                    toFloat (round (60 + rnd (s + 2) * 140))

                half =
                    toFloat (round (len / 2))

                dy =
                    if rnd (s + 3) < 0.5 then
                        -12

                    else
                        12

                k =
                    modFloat (time * (40 + 30 * rnd (s + 4)) + rnd (s + 5) * 300) (len + 60)

                ( px, py ) =
                    if k < half then
                        ( x0 + k, y )

                    else
                        ( x0 + k, y + dy )

                traceColor =
                    "#0c3c50"
            in
            [ Rect (x0 - 1) (y - 1) 3 3 traceColor
            , Rect x0 y half 1 traceColor
            , Rect (x0 + half) (min y (y + dy)) 1 (abs dy) traceColor
            , Rect (x0 + half) (y + dy) half 1 traceColor
            , Rect (x0 + len - 1) (y + dy - 1) 3 3 traceColor
            ]
                ++ (if k < len then
                        [ Rect (px - 3) py 3 1 "#007c8c", Rect px py 3 1 "#00e8d8" ]

                    else
                        []
                   )

        period =
            1.7

        cycle =
            floor (time / period)

        zap =
            if time - toFloat cycle * period < 0.12 then
                let
                    s =
                        cycle * 31

                    pts =
                        List.map
                            (\j -> ( rnd s * gameW + rndRange (s + j) -10 10, 24 + toFloat j * (14 + rnd (s + 1) * 10) ))
                            (List.range 0 6)

                    segs =
                        List.map2 Tuple.pair pts (List.drop 1 pts)
                in
                List.concatMap (\( ( x1, y1 ), ( x2, y2 ) ) -> [ Line x1 y1 x2 y2 3 "#3cbcfc", Line x1 y1 x2 y2 1 "#fcfcfc" ]) segs

            else
                []
    in
    List.concatMap trace (List.range 0 11) ++ zap
