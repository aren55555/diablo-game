module Fx exposing
    ( Target
    , alivePlayers
    , burst
    , damageTarget
    , drainPlayer
    , dropItem
    , explode
    , findPlayer
    , floatText
    , hurtPlayer
    , makeZone
    , mapPlayer
    , nearestPlayer
    , particle
    , pcx
    , pcy
    , pickSeed
    , pickup
    , playerBox
    , sfx
    , shake
    , spawnEnemy
    , spawnProj
    , targets
    )

{-| Shared world effects: sounds, particles, damage, spawning.
-}

import Make
import Types exposing (..)
import Util exposing (..)


sfx : String -> World -> World
sfx s w =
    { w | sfx = s :: w.sfx }


shake : Float -> World -> World
shake amount w =
    { w | shake = max w.shake amount }


{-| A fresh seed for random effects; also bumps the id counter.
-}
pickSeed : World -> ( Int, World )
pickSeed w =
    ( w.nextId * 7919 + w.tick * 104729, { w | nextId = w.nextId + 1 } )


particle : Particle -> World -> World
particle p w =
    { w | particles = p :: w.particles }


burst : Float -> Float -> String -> Int -> Float -> Float -> World -> World
burst x y color n speed size w0 =
    let
        ( seed, w ) =
            pickSeed w0

        mk i =
            let
                s =
                    seed + i * 5

                a =
                    rnd s * 2 * pi

                v =
                    (0.3 + 0.7 * rnd (s + 1)) * speed

                l =
                    0.25 + 0.4 * rnd (s + 2)
            in
            { x = x, y = y, vx = cos a * v, vy = sin a * v, life = l, maxLife = l, size = size * (0.6 + 0.7 * rnd (s + 3)), grav = 160, color = color, kind = Dot }
    in
    { w | particles = List.map mk (List.range 0 (n - 1)) ++ w.particles }


floatText : Float -> Float -> String -> String -> World -> World
floatText x y text color =
    particle { x = x, y = y, vx = 0, vy = -24, life = 1, maxLife = 1, size = 1, grav = 0, color = color, kind = Txt text }



-- PLAYERS --------------------------------------------------------------------


pcx : Player -> Float
pcx p =
    p.x + 5


pcy : Player -> Float
pcy p =
    p.y + 7.5


playerBox : Player -> { x : Float, y : Float, w : Float, h : Float }
playerBox p =
    { x = p.x, y = p.y, w = 10, h = 15 }


alivePlayers : World -> List Player
alivePlayers w =
    List.filter (\p -> p.hp > 0) w.players


findPlayer : Int -> World -> Maybe Player
findPlayer idx w =
    List.head (List.filter (\p -> p.idx == idx) w.players)


mapPlayer : Int -> (Player -> Player) -> World -> World
mapPlayer idx f w =
    { w
        | players =
            List.map
                (\p ->
                    if p.idx == idx then
                        f p

                    else
                        p
                )
                w.players
    }


nearestPlayer : Float -> Float -> World -> Maybe Player
nearestPlayer x y w =
    alivePlayers w
        |> List.sortBy (\p -> (pcx p - x) ^ 2 + (pcy p - y) ^ 2)
        |> List.head


isCleared : World -> Bool
isCleared w =
    case w.phase of
        Cleared _ ->
            True

        _ ->
            False


knockOut : Player -> World -> World
knockOut p w =
    w
        |> sfx "ko"
        |> burst (pcx p) (pcy p) "#fcfcfc" 20 120 2
        |> floatText (pcx p) (p.y - 8) ("P" ++ String.fromInt (p.idx + 1) ++ " KO!") "#fc7460"
        |> mapPlayer p.idx (\q -> { q | slow = 0, poison = 0, ghostT = 0 })


{-| Hit with i-frames and knockback (from source x if given).
-}
hurtPlayer : Int -> Float -> Maybe Float -> World -> World
hurtPlayer idx dmg src w =
    case findPlayer idx w of
        Just p ->
            if p.hp <= 0 || p.invuln > 0 || isCleared w then
                w

            else
                let
                    hp2 =
                        max 0 (p.hp - dmg)

                    ( vx, vy ) =
                        case src of
                            Just sx ->
                                ( if pcx p < sx then
                                    -112

                                  else
                                    112
                                , min p.vy -132
                                )

                            Nothing ->
                                ( p.vx, p.vy )

                    w2 =
                        w
                            |> mapPlayer idx (\q -> { q | hp = hp2, invuln = 1.0, hurtFx = 0.25, vx = vx, vy = vy })
                            |> sfx "hurt"
                            |> shake 2
                            |> floatText (pcx p) (p.y - 6) ("-" ++ String.fromInt (round dmg)) "#fc7460"
                in
                if hp2 <= 0 then
                    knockOut p w2

                else
                    w2

        Nothing ->
            w


{-| Continuous damage (breath, beams, poison) that ignores i-frames.
-}
drainPlayer : Int -> Float -> World -> World
drainPlayer idx amount w =
    case findPlayer idx w of
        Just p ->
            if p.hp <= 0 || isCleared w then
                w

            else
                let
                    hp2 =
                        max 0 (p.hp - amount)

                    w2 =
                        mapPlayer idx (\q -> { q | hp = hp2, hurtFx = 0.1 }) w
                in
                if hp2 <= 0 then
                    knockOut p w2

                else
                    w2

        Nothing ->
            w



-- TARGETS & DAMAGE -------------------------------------------------------------


type alias Target =
    { id : Int, x : Float, y : Float, w : Float, h : Float }


{-| Things players can hit. The boss has id -1.
-}
targets : World -> List Target
targets w =
    let
        es =
            w.enemies
                |> List.filter (\e -> not e.dead && e.active)
                |> List.map (\e -> { id = e.id, x = e.x, y = e.y, w = e.w, h = e.h })
    in
    case w.boss of
        Just b ->
            if b.state == Fighting then
                { id = -1, x = b.x, y = b.y, w = b.w, h = b.h } :: es

            else
                es

        Nothing ->
            es


damageTarget : Int -> Float -> Float -> Int -> Bool -> World -> World
damageTarget id dmg kb pi quiet w =
    if id == -1 then
        case w.boss of
            Just b ->
                if b.state /= Fighting then
                    w

                else
                    let
                        hp2 =
                            b.hp - dmg

                        enrage =
                            not b.enraged && hp2 < b.maxHp / 2

                        w2 =
                            { w | boss = Just { b | hp = hp2, flash = 0.08, enraged = b.enraged || enrage } }
                                |> numberText (b.x + b.w / 2) b.y dmg quiet
                    in
                    if enrage then
                        { w2 | banner = Just { text = b.def.name ++ " IS ANGRY!", sub = "WATCH OUT!", t = 2, color = "#fc3c3c" } }
                            |> sfx "roar"

                    else
                        w2

            Nothing ->
                w

    else
        case List.head (List.filter (\e -> e.id == id) w.enemies) of
            Just e ->
                if e.dead then
                    w

                else
                    let
                        hp2 =
                            e.hp - dmg

                        e2 =
                            { e | hp = hp2, flash = 0.12, vx = e.vx + kb, dead = hp2 <= 0 }

                        w2 =
                            { w
                                | enemies =
                                    List.map
                                        (\x ->
                                            if x.id == id then
                                                e2

                                            else
                                                x
                                        )
                                        w.enemies
                            }
                                |> numberText (e.x + e.w / 2) e.y dmg quiet
                    in
                    if hp2 <= 0 then
                        killEnemy e2 pi w2

                    else
                        w2

            Nothing ->
                w


numberText : Float -> Float -> Float -> Bool -> World -> World
numberText x y dmg quiet w =
    if quiet || dmg < 8 then
        w

    else
        floatText (x + rndRange (w.tick + round x) -4 4) (y - 3) (String.fromInt (round dmg)) "#fce4a0" w


killEnemy : Enemy -> Int -> World -> World
killEnemy e pi w =
    let
        color =
            case e.kind of
                BabyDragon ->
                    "#58d854"

                Archer ->
                    "#fcfcfc"

                Goon ->
                    "#fc9838"

        r =
            rnd (e.id * 13 + w.tick)

        cx =
            e.x + e.w / 2

        withDrop =
            if r < 0.15 then
                dropItem Heart cx e.y

            else if r < 0.7 then
                dropItem Coin cx e.y

            else
                identity
    in
    w
        |> sfx "enemyDie"
        |> burst cx (e.y + e.h / 2) color 14 104 2
        |> mapPlayer pi (\p -> { p | kills = p.kills + 1 })
        |> withDrop


dropItem : ItemKind -> Float -> Float -> World -> World
dropItem kind x y w0 =
    let
        ( seed, w ) =
            pickSeed w0

        it =
            Make.item w.nextId kind (x - 4) (y - 4)
    in
    { w
        | items = { it | vx = rndRange seed -48 48, vy = rndRange (seed + 1) -168 -100, phys = True, life = 14 } :: w.items
        , nextId = w.nextId + 1
    }


pickup : ItemKind -> Float -> Float -> Float -> Float -> Float -> World -> World
pickup kind x y vx vy life w =
    let
        it =
            Make.item w.nextId kind x y
    in
    { w | items = { it | vx = vx, vy = vy, phys = True, life = life } :: w.items, nextId = w.nextId + 1 }


spawnProj : Proj -> World -> World
spawnProj p w =
    { w | projs = { p | id = w.nextId } :: w.projs, nextId = w.nextId + 1 }


spawnEnemy : EnemyKind -> Float -> Float -> Int -> Int -> World -> World
spawnEnemy kind x y plat lvl w =
    let
        e =
            Make.enemy w.nextId kind x y plat lvl
    in
    { w | enemies = { e | active = True } :: w.enemies, nextId = w.nextId + 1 }
        |> burst (x + e.w / 2) (e.y + e.h / 2) "#fcfcfc" 10 80 2


explode : Float -> Float -> Float -> Float -> Int -> World -> World
explode x y radius dmg pi w =
    let
        hit t =
            let
                tx =
                    clamp t.x (t.x + t.w) x

                ty =
                    clamp t.y (t.y + t.h) y
            in
            sqrt ((tx - x) ^ 2 + (ty - y) ^ 2) < radius
    in
    List.foldl
        (\t acc -> damageTarget t.id dmg (sign (t.x + t.w / 2 - x) * 80) pi False acc)
        w
        (List.filter hit (targets w))
        |> sfx "boom"
        |> shake 4
        |> particle { x = x, y = y, vx = 0, vy = 0, life = 0.35, maxLife = 0.35, size = radius, grav = 0, color = "#fce4a0", kind = Ring }
        |> burst x y "#fc7c00" 16 130 3
        |> burst x y "#fcfc58" 8 80 2
        |> burst x y "#404048" 8 50 4


makeZone : Float -> Float -> Float -> Float -> Float -> Bool -> World -> World
makeZone x y r life dps hostile w =
    { w | zones = { x = x, y = y, r = r, life = life, maxLife = life, dps = dps, hostile = hostile, t = 0 } :: w.zones }
