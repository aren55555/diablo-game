module Level exposing (Generated, generate)

{-| Procedural level generation. Each level is a vertical climb of one-way
platforms ending in a full-width boss arena. Difficulty grows with n.
-}

import Data
import Make
import Types exposing (..)
import Util exposing (..)


type alias Generated =
    { level : Level, enemies : List Enemy, items : List Item, nextId : Int }


type alias Gen =
    { seed : Int, plats : List Platform, nextId : Int }


{-| Draw a random number in [a, b) and advance the seed.
-}
roll : Float -> Float -> Gen -> ( Float, Gen )
roll a b g =
    ( rndRange g.seed a b, { g | seed = g.seed + 1 } )


plat : Int -> Float -> Float -> Float -> Platform
plat id x y w =
    { id = id, x = x, y = y, w = w, h = 6, kind = Normal, moving = Nothing, crumble = False, ct = -1, gone = False, respawnT = 0, dx = 0, ice = False }


generate : Int -> Int -> Generated
generate n numPlayers =
    let
        info =
            Data.levelInfo n

        nf =
            toFloat n

        height =
            if n >= 11 then
                360

            else
                600 + 112 * (nf - 1)

        arenaY =
            -height

        minW =
            max 34 (76 - nf * 3.6)

        maxW =
            max 52 (100 - nf * 3.2)

        ground =
            { id = 0, x = 0, y = 0, w = gameW, h = 30, kind = Ground, moving = Nothing, crumble = False, ct = -1, gone = False, respawnT = 0, dx = 0, ice = False }

        climb y px g =
            if y <= arenaY + 56 then
                ( g, y, px )

            else
                let
                    ( pw, g1 ) =
                        roll minW maxW g

                    ( dirR, g2 ) =
                        roll 0 1 g1

                    ( off, g3 ) =
                        roll 36 92 g2

                    cx0 =
                        px
                            + (if dirR < 0.5 then
                                -off

                               else
                                off
                              )

                    cx1 =
                        if cx0 < pw / 2 + 4 || cx0 > gameW - pw / 2 - 4 then
                            px - (cx0 - px)

                        else
                            cx0

                    cx =
                        clamp (pw / 2 + 4) (gameW - pw / 2 - 4) cx1

                    ( kindR, g4 ) =
                        roll 0 1 g3

                    ( range, g5 ) =
                        roll 24 56 g4

                    ( speed, g6 ) =
                        roll 0.6 1.1 g5

                    ( phase, g7 ) =
                        roll 0 (2 * pi) g6

                    ( iceR, g8 ) =
                        roll 0 1 g7

                    moveChance =
                        if n >= 3 then
                            0.08 + 0.025 * nf

                        else
                            0

                    crumbleChance =
                        if n >= 5 then
                            moveChance + 0.04 * (nf - 4)

                        else
                            0

                    base =
                        plat g8.nextId (cx - pw / 2) y pw

                    p =
                        { base
                            | moving =
                                if kindR < moveChance then
                                    Just { x0 = cx - pw / 2, range = range, speed = speed + nf * 0.05, phase = phase }

                                else
                                    Nothing
                            , crumble = kindR >= moveChance && kindR < crumbleChance
                            , ice = info.theme.ice && iceR < 0.5
                        }

                    ( sideR, g9 ) =
                        roll 0 1 { g8 | nextId = g8.nextId + 1, plats = p :: g8.plats }

                    ( pw2, g10 ) =
                        roll (minW * 0.8) (maxW * 0.8) g9

                    ( off2, g11 ) =
                        roll 104 168 g10

                    ( dy2, g12 ) =
                        roll -12 12 g11

                    cx2 =
                        if cx < gameW / 2 then
                            cx + off2

                        else
                            cx - off2

                    g13 =
                        if sideR < 0.45 && cx2 > pw2 / 2 + 4 && cx2 < gameW - pw2 / 2 - 4 then
                            { g12 | plats = plat g12.nextId (cx2 - pw2 / 2) (y + dy2) pw2 :: g12.plats, nextId = g12.nextId + 1 }

                        else
                            g12

                    ( gap, g14 ) =
                        roll 40 51 g13
                in
                climb (y - gap - min nf 10) cx g14

        ( gEnd, _, _ ) =
            climb -48 (gameW / 2) { seed = n * 100003 + 17, plats = [], nextId = 1 }

        -- make sure the arena is always reachable with a double jump
        topPlat =
            List.head (List.sortBy .y gEnd.plats)

        gBridge =
            case topPlat of
                Just tp ->
                    if tp.y - arenaY > 70 then
                        { gEnd | plats = plat gEnd.nextId (clamp 4 (gameW - 60) (tp.x + tp.w / 2 - 28)) ((tp.y + arenaY) / 2) 56 :: gEnd.plats, nextId = gEnd.nextId + 1 }

                    else
                        gEnd

                Nothing ->
                    gEnd

        arena =
            { id = gBridge.nextId, x = 0, y = arenaY, w = gameW, h = 26, kind = Arena, moving = Nothing, crumble = False, ct = -1, gone = False, respawnT = 0, dx = 0, ice = False }

        climbPlats =
            List.reverse gBridge.plats

        platforms =
            ground :: climbPlats ++ [ arena ]

        ( items, id2 ) =
            placeItems n numPlayers info.weapons climbPlats (gBridge.nextId + 1)

        ( enemies, id3 ) =
            placeEnemies n climbPlats arenaY id2
    in
    { level =
        { n = n
        , name = info.name
        , theme = info.theme
        , platforms = platforms
        , height = height
        , arenaY = arenaY
        , arenaCamTop = arenaY - 210
        , hazards = n >= 6 && n <= 10
        }
    , enemies = enemies
    , items = items
    , nextId = id3
    }


safe : Platform -> Bool
safe p =
    p.moving == Nothing && not p.crumble


placeItems : Int -> Int -> List Weapon -> List Platform -> Int -> ( List Item, Int )
placeItems n numPlayers pool plats startId =
    let
        nf =
            toFloat n

        perPlat ( p, i ) ( acc, id ) =
            let
                s =
                    n * 7777 + i * 31

                cx =
                    p.x + p.w / 2

                coins =
                    if rnd s < 0.45 then
                        let
                            k =
                                3 + floor (rnd (s + 1) * 3)
                        in
                        List.map (\j -> Make.item (id + j) Coin (cx - toFloat k * 4.5 + toFloat j * 9 - 4) (p.y - 14)) (List.range 0 (k - 1))

                    else
                        []

                idC =
                    id + List.length coins

                extra =
                    if rnd (s + 2) < 0.07 then
                        [ Make.item idC Diamond (cx - 4) (p.y - 24) ]

                    else if rnd (s + 3) < 0.07 + 0.006 * nf then
                        [ Make.item idC Heart (cx - 4) (p.y - 24) ]

                    else
                        []
            in
            ( coins ++ extra ++ acc, idC + 1 )

        ( pickups, id1 ) =
            List.foldl perPlat ( [], startId ) (List.indexedMap (\i p -> ( p, i )) plats)

        safePlats =
            List.filter safe plats

        count =
            List.length safePlats

        nWeapons =
            numPlayers + 1

        weaponsPlaced =
            if n >= 11 then
                -- an armory on the ground before the final fight
                List.indexedMap (\j wp -> Make.item (id1 + j) (WeaponItem wp) (40 + toFloat j * 64) -18) pool

            else
                List.filterMap
                    (\j ->
                        let
                            frac =
                                (toFloat j + 0.6) / (toFloat nWeapons + 0.6)

                            idx =
                                clamp 0 (count - 1) (round (frac * toFloat (count - 1)))

                            wp =
                                Maybe.withDefault Nunchucks (listGet (modBy (List.length pool) (j + n)) pool)
                        in
                        listGet idx safePlats
                            |> Maybe.map (\p -> Make.item (id1 + j) (WeaponItem wp) (p.x + p.w / 2 - 6) (p.y - 12))
                    )
                    (List.range 0 (nWeapons - 1))
    in
    ( weaponsPlaced ++ pickups, id1 + List.length pool + nWeapons + 1 )


placeEnemies : Int -> List Platform -> Float -> Int -> ( List Enemy, Int )
placeEnemies n plats arenaY startId =
    let
        nf =
            toFloat n

        walkers ( p, i ) ( acc, id ) =
            let
                s =
                    n * 3331 + i * 17
            in
            if p.y > -140 || not (safe p) || p.w < 44 || n >= 11 then
                ( acc, id )

            else if rnd s < 0.12 + 0.035 * nf then
                ( Make.enemy id Goon (p.x + p.w / 2) p.y p.id n :: acc, id + 1 )

            else if n >= 4 && rnd (s + 1) < 0.04 * (nf - 3) then
                ( Make.enemy id Archer (p.x + p.w / 2) p.y p.id n :: acc, id + 1 )

            else
                ( acc, id )

        ( ground, id1 ) =
            List.foldl walkers ( [], startId ) (List.indexedMap (\i p -> ( p, i )) plats)

        babies =
            if n >= 11 then
                2

            else
                min 6 (round (1.5 + nf * 0.6))

        flyers =
            List.map
                (\j ->
                    let
                        s =
                            n * 999 + j * 7
                    in
                    Make.enemy (id1 + j) BabyDragon (rndRange s 30 330) (rndRange (s + 1) (arenaY + 120) -160) -1 n
                )
                (List.range 0 (babies - 1))
    in
    ( flyers ++ ground, id1 + babies )
