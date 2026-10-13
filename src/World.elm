module World exposing (new, reviveTime, screenFloor, skipToBoss, step)

{-| The per-frame game simulation for one level.
-}

import Aura
import Boss
import Data
import Fx exposing (..)
import Level
import Make
import Types exposing (..)
import Util exposing (..)


grav : Float
grav =
    800


jumpV : Float
jumpV =
    304


djumpV : Float
djumpV =
    256


maxFall : Float
maxFall =
    380


{-| Start level n with the given (persistent) players. Knocked-out players
come back with half health.
-}
new : Int -> List Player -> World
new n players =
    let
        gen =
            Level.generate n (List.length players)

        count =
            List.length players

        place i p =
            { p
                | x = gameW / 2 - toFloat count * 14 + toFloat i * 28
                , y = -15
                , vx = 0
                , vy = 0
                , hp =
                    if p.hp <= 0 then
                        50

                    else
                        p.hp
                , invuln = 1.5
                , slow = 0
                , poison = 0
                , standing = -1
                , onGround = False
                , atkT = 0
                , cd = 0
            }

        bossDef =
            Data.bossFor n
    in
    { level = gen.level
    , players = List.indexedMap place players
    , enemies = gen.enemies
    , projs = []
    , zones = []
    , beams = []
    , particles = []
    , items = gen.items
    , boss = Just (Make.boss n bossDef gen.level.arenaY count)
    , camY = -210
    , shake = 0
    , phase = Climb
    , banner = Just { text = "LEVEL " ++ String.fromInt n, sub = gen.level.name, t = 2.5, color = "#fcfcfc" }
    , time = 0
    , tick = 0
    , nextId = gen.nextId + 10
    , sfx = []
    , hazardT = 4
    , heartT = 12
    , warnings = []
    , outcome = Ongoing
    }


{-| Advance the world by dt seconds. pads are indexed by player idx (0-3).
-}
step : Float -> (Int -> Pad) -> World -> World
step dt pads w0 =
    let
        w =
            { w0 | sfx = [], time = w0.time + dt, tick = w0.tick + 1, shake = max 0 (w0.shake - dt * 30) }
    in
    w
        |> updatePlatforms dt
        |> updatePlayers dt pads
        |> updateCamera dt
        |> handleFallen
        |> updateEnemies dt
        |> Boss.update dt
        |> updateProjs dt
        |> updateZones dt
        |> updateItems dt
        |> updateParticles dt
        |> hazards dt
        |> phaseLogic dt
        |> cleanup



-- PLATFORMS --------------------------------------------------------------------


updatePlatforms : Float -> World -> World
updatePlatforms dt w =
    let
        lvl =
            w.level

        upd p =
            let
                moved =
                    case p.moving of
                        Just m ->
                            let
                                nx =
                                    m.x0 + sin (w.time * m.speed + m.phase) * m.range
                            in
                            { p | x = nx, dx = nx - p.x }

                        Nothing ->
                            p
            in
            if moved.gone then
                if moved.respawnT - dt <= 0 then
                    { moved | gone = False, ct = -1, respawnT = 0 }

                else
                    { moved | respawnT = moved.respawnT - dt }

            else if moved.ct >= 0 then
                if moved.ct + dt > 0.55 then
                    { moved | gone = True, ct = -1, respawnT = 3.5 }

                else
                    { moved | ct = moved.ct + dt }

            else
                moved
    in
    { w | level = { lvl | platforms = List.map upd lvl.platforms } }


platformById : Int -> World -> Maybe Platform
platformById id w =
    if id < 0 then
        Nothing

    else
        List.head (List.filter (\p -> p.id == id) w.level.platforms)



-- PLAYERS ----------------------------------------------------------------------


updatePlayers : Float -> (Int -> Pad) -> World -> World
updatePlayers dt pads w =
    List.foldl (\p acc -> updatePlayer dt (pads p.idx) p.idx acc) w w.players


updatePlayer : Float -> Pad -> Int -> World -> World
updatePlayer dt pad idx w =
    case findPlayer idx w of
        Nothing ->
            w

        Just p0 ->
            let
                p =
                    { p0
                        | anim = p0.anim + dt
                        , invuln = p0.invuln - dt
                        , hurtFx = p0.hurtFx - dt
                        , atkT = p0.atkT - dt
                        , cd = p0.cd - dt
                        , prompt = p0.prompt - dt
                    }
            in
            if p.hp <= 0 then
                ghost dt p w

            else
                alive dt pad p w


reviveTime : Float
reviveTime =
    10


ghost : Float -> Player -> World -> World
ghost dt p w =
    let
        others =
            alivePlayers w

        n =
            toFloat (List.length others)

        tx =
            if n > 0 then
                List.sum (List.map pcx others) / n + (toFloat p.idx - 1.5) * 16

            else
                pcx p

        ty =
            (List.minimum (List.map .y others) |> Maybe.withDefault p.y) - 28

        k =
            min 1 (dt * 2)
    in
    -- ghosts come back after a while as long as a teammate is still standing
    if p.ghostT + dt > reviveTime && n > 0 then
        let
            buddy =
                List.head (List.sortBy (\q -> abs (q.x - p.x)) others)

            ( rx, ry ) =
                case buddy of
                    Just q ->
                        ( q.x, q.y - 4 )

                    Nothing ->
                        ( p.x, p.y )
        in
        w
            |> mapPlayer p.idx (\_ -> { p | hp = 30, x = rx, y = ry, vx = 0, vy = -150, invuln = 2, ghostT = 0, onGround = False, standing = -1 })
            |> sfx "heart"
            |> sfx (Aura.voice p)
            |> burst (rx + 5) (ry + 8) "#fcfcfc" 16 90 2
            |> floatText (rx + 5) (ry - 8) ("P" ++ String.fromInt (p.idx + 1) ++ " IS BACK!") "#58d854"

    else
        mapPlayer p.idx
            (\_ ->
                { p
                    | ghostT = p.ghostT + dt
                    , x = lerp p.x (tx - 5) k
                    , y = lerp p.y (ty + sin (p.ghostT * 3 + toFloat p.idx) * 4) k
                }
            )
            w


alive : Float -> Pad -> Player -> World -> World
alive dt pad p0 w0 =
    let
        -- status effects
        ( p1, w1 ) =
            statusEffects dt p0 w0

        regen =
            if w1.phase == Climb then
                1.5 * dt

            else
                0

        ride =
            platformById p1.standing w1 |> Maybe.map .dx |> Maybe.withDefault 0

        speedMul =
            if p1.slow > 0 then
                0.5

            else
                1

        ice =
            platformById p1.standing w1 |> Maybe.map .ice |> Maybe.withDefault False

        ( accel, fric ) =
            if ice then
                ( 360, 140 )

            else
                ( 1040, 960 )

        ax =
            (if pad.left then
                -1

             else
                0
            )
                + (if pad.right then
                    1

                   else
                    0
                  )

        maxV =
            108 * speedMul

        vx1 =
            if ax /= 0 then
                let
                    v =
                        p1.vx + ax * accel * dt
                in
                if abs v > maxV then
                    lerp v (sign v * maxV) (min 1 (dt * 12))

                else
                    v

            else
                sign p1.vx * max 0 (abs p1.vx - fric * dt)

        facing =
            if ax /= 0 then
                ax

            else
                p1.facing

        -- jumping: coyote time, jump buffer and a double jump
        jumpBuf =
            if pad.upP then
                0.13

            else
                p1.jumpBuf - dt

        coyote =
            if p1.onGround then
                0.1

            else
                p1.coyote - dt

        ( vy1, jumpState, jumpFx ) =
            if jumpBuf > 0 && coyote > 0 then
                ( -jumpV, { jumpBuf = 0, coyote = 0, airJumps = 1 }, sfx "jump" )

            else if jumpBuf > 0 && p1.airJumps > 0 then
                ( -djumpV
                    * (if p1.slow > 0 then
                        0.85

                       else
                        1
                      )
                , { jumpBuf = 0, coyote = coyote, airJumps = p1.airJumps - 1 }
                , sfx "djump" >> burst (pcx p1) (p1.y + 15) "#fcfcfc" 6 40 1.5
                )

            else
                ( p1.vy, { jumpBuf = jumpBuf, coyote = coyote, airJumps = p1.airJumps }, identity )

        vy2 =
            if not pad.up && vy1 < -100 then
                vy1 + grav * dt * 1.4

            else
                vy1

        vy3 =
            min maxFall (vy2 + grav * dt)

        x2 =
            clamp 1 (gameW - 11) (p1.x + ride + vx1 * dt)

        prevBottom =
            p1.y + 15

        y2 =
            p1.y + vy3 * dt

        landing =
            if vy3 >= 0 then
                w1.level.platforms
                    |> List.filter (\pl -> not pl.gone && x2 + 10 > pl.x + 1 && x2 < pl.x + pl.w - 1 && prevBottom <= pl.y + 1 && y2 + 15 >= pl.y)
                    |> List.head

            else
                Nothing

        moved =
            case landing of
                Just pl ->
                    { p1 | x = x2, y = pl.y - 15, vx = vx1, vy = 0, onGround = True, standing = pl.id, airJumps = 1, facing = facing, jumpBuf = jumpState.jumpBuf, coyote = jumpState.coyote, hp = min 100 (p1.hp + regen) }

                Nothing ->
                    { p1 | x = x2, y = y2, vx = vx1, vy = vy3, onGround = False, standing = -1, airJumps = jumpState.airJumps, facing = facing, jumpBuf = jumpState.jumpBuf, coyote = jumpState.coyote, hp = min 100 (p1.hp + regen) }

        -- the top of the screen is a ceiling: the group climbs together
        capped =
            if moved.y < w1.camY + 22 then
                { moved | y = w1.camY + 22, vy = max 0 moved.vy }

            else
                moved

        -- the bottom of the screen is a floor too: nobody falls out of view
        floored =
            if capped.vy >= 0 && capped.y + 15 > screenFloor w1 then
                { capped | y = screenFloor w1 - 15, vy = 0, onGround = True, standing = -1, airJumps = 1 }

            else
                capped

        w2 =
            w1
                |> mapPlayer p1.idx (\_ -> floored)
                |> jumpFx
                |> triggerCrumble landing
    in
    w2
        |> grabOrAttack pad floored
        |> Aura.update dt pad.downP floored.idx


triggerCrumble : Maybe Platform -> World -> World
triggerCrumble landing w =
    case landing of
        Just pl ->
            if pl.crumble && pl.ct < 0 then
                let
                    lvl =
                        w.level
                in
                { w
                    | level =
                        { lvl
                            | platforms =
                                List.map
                                    (\q ->
                                        if q.id == pl.id then
                                            { q | ct = 0 }

                                        else
                                            q
                                    )
                                    lvl.platforms
                        }
                }
                    |> sfx "crumble"

            else
                w

        Nothing ->
            w


statusEffects : Float -> Player -> World -> ( Player, World )
statusEffects dt p w =
    let
        s =
            w.tick * 11 + p.idx

        w1 =
            if p.slow > 0 && rnd s < dt * 10 then
                particle { x = p.x + rnd (s + 1) * 10, y = p.y + rnd (s + 2) * 15, vx = 0, vy = 8, life = 0.5, maxLife = 0.5, size = 1, grav = 0, color = "#c4f0fc", kind = Dot } w

            else
                w

        w2 =
            if p.poison > 0 && rnd (s + 3) < dt * 8 then
                particle { x = p.x + rnd (s + 4) * 10, y = p.y + rnd (s + 5) * 15, vx = 0, vy = -16, life = 0.7, maxLife = 0.7, size = 1.5, grav = 0, color = "#b8f818", kind = Dot } w1

            else
                w1

        p2 =
            { p | slow = max 0 (p.slow - dt), poison = max 0 (p.poison - dt) }

        w3 =
            mapPlayer p.idx (\_ -> p2) w2
    in
    if p.poison > 0 then
        let
            w4 =
                drainPlayer p.idx (3 * dt) w3
        in
        ( Maybe.withDefault p2 (findPlayer p.idx w4), w4 )

    else
        ( p2, w3 )


grabOrAttack : Pad -> Player -> World -> World
grabOrAttack pad p w =
    if p.hp <= 0 then
        w

    else
        let
            touching =
                w.items
                    |> List.filter (\it -> not it.gone && overlaps (playerBox p) (itemBox it))
                    |> List.filterMap
                        (\it ->
                            case it.kind of
                                WeaponItem wp ->
                                    Just ( it, wp )

                                _ ->
                                    Nothing
                        )
                    |> List.head
        in
        case touching of
            Just ( it, wp ) ->
                if p.weapon == Fists || pad.downP then
                    let
                        old =
                            p.weapon

                        swapItem i =
                            if i.id /= it.id then
                                i

                            else if old == Fists then
                                { i | gone = True }

                            else
                                { i | kind = WeaponItem old, vy = -120, phys = True, t = 0, life = 0 }
                    in
                    { w | items = List.map swapItem w.items }
                        |> mapPlayer p.idx (\q -> { q | weapon = wp, cd = 0.35 })
                        |> sfx "pickup"
                        |> floatText (pcx p) (p.y - 8) ((Data.weaponDef wp).name ++ "!") "#fcfcfc"

                else
                    let
                        w2 =
                            mapPlayer p.idx (\q -> { q | prompt = 0.1 }) w
                    in
                    if pad.down && p.cd <= 0 then
                        attack p w2

                    else
                        w2

            Nothing ->
                if pad.down && p.cd <= 0 then
                    attack p w

                else
                    w


itemBox : Item -> { x : Float, y : Float, w : Float, h : Float }
itemBox it =
    case it.kind of
        WeaponItem _ ->
            { x = it.x - 2, y = it.y - 4, w = 16, h = 14 }

        _ ->
            { x = it.x, y = it.y, w = 8, h = 8 }


autoAim : Player -> Float -> World -> Float
autoAim p maxAng w =
    let
        ox =
            pcx p

        oy =
            p.y + 7

        candidates =
            targets w
                |> List.filterMap
                    (\t ->
                        let
                            dx =
                                t.x + t.w / 2 - ox

                            dy =
                                t.y + t.h / 2 - oy

                            d =
                                sqrt (dx * dx + dy * dy)

                            a =
                                atan2 dy (abs dx)
                        in
                        if dx * p.facing < -2 || d > 304 || abs a > maxAng + 0.35 then
                            Nothing

                        else
                            Just ( d, a )
                    )
                |> List.sortBy Tuple.first

        a0 =
            case candidates of
                ( _, a ) :: _ ->
                    clamp -maxAng maxAng a

                [] ->
                    0
    in
    if p.facing > 0 then
        a0

    else
        pi - a0


melee : Player -> { x : Float, y : Float, w : Float, h : Float } -> Float -> Float -> World -> World
melee p box dmg kb w =
    List.foldl
        (\t acc ->
            if overlaps box t then
                acc
                    |> damageTarget t.id dmg (kb * sign (t.x + t.w / 2 - pcx p)) p.idx False
                    |> burst (clamp t.x (t.x + t.w) (pcx p + p.facing * 8)) (p.y + 7) "#fcfcfc" 4 72 1.5

            else
                acc
        )
        w
        (targets w)


attack : Player -> World -> World
attack p w0 =
    let
        def =
            Data.weaponDef p.weapon

        w =
            mapPlayer p.idx (\q -> { q | cd = def.cd, atkT = def.anim }) w0

        f =
            p.facing

        ox =
            pcx p + f * 6

        oy =
            p.y + 7

        cx =
            pcx p

        shot =
            Make.proj
    in
    case p.weapon of
        Fists ->
            w
                |> melee p
                    { x =
                        if f > 0 then
                            cx

                        else
                            cx - 16
                    , y = p.y + 1
                    , w = 16
                    , h = 14
                    }
                    10
                    64
                |> sfx "punch"

        Nunchucks ->
            w
                |> melee p { x = cx - 26, y = p.y - 2, w = 52, h = 20 } 13 56
                |> sfx "swish"

        Sword ->
            let
                a =
                    autoAim p 0.5 w
            in
            w
                |> melee p
                    { x =
                        if f > 0 then
                            cx

                        else
                            cx - 34
                    , y = p.y - 10
                    , w = 34
                    , h = 30
                    }
                    26
                    88
                |> spawnProj { shot | owner = ByPlayer p.idx, kind = Magic, x = ox, y = oy, vx = cos a * 256, vy = sin a * 256, r = 5, dmg = 12, life = 0.45, pierce = True }
                |> sfx "sword"

        MachineGun ->
            let
                a =
                    autoAim p 0.65 w + rndRange (w.tick + p.idx) -0.06 0.06
            in
            w
                |> spawnProj { shot | owner = ByPlayer p.idx, kind = Bullet, x = ox + f * 4, y = oy, vx = cos a * 392, vy = sin a * 392, r = 2, dmg = 6, life = 0.8 }
                |> particle { x = ox + f * 8, y = oy, vx = 0, vy = 0, life = 0.05, maxLife = 0.05, size = 3, grav = 0, color = "#fcfc58", kind = Dot }
                |> sfx "gun"

        Bazooka ->
            let
                a =
                    autoAim p 0.65 w
            in
            w
                |> spawnProj { shot | owner = ByPlayer p.idx, kind = Rocket, x = ox + f * 6, y = oy - 2, vx = cos a * 224, vy = sin a * 224, r = 3, dmg = 40, life = 1.6, explode = 38 }
                |> mapPlayer p.idx (\q -> { q | vx = q.vx - f * 64 })
                |> sfx "rocket"

        FartBomb ->
            let
                a =
                    autoAim p 0.4 w
            in
            w
                |> spawnProj { shot | owner = ByPlayer p.idx, kind = FartP, x = ox, y = oy - 3, vx = cos a * 136, vy = sin a * 136 - 144, r = 4, dmg = 0, life = 1.2, grav = 440 }
                |> sfx "swish"

        Laser ->
            let
                a =
                    autoAim p 0.65 w

                len =
                    330

                x2 =
                    ox + cos a * len

                y2 =
                    oy + sin a * len

                hits =
                    List.filter (\t -> pointSegDist (t.x + t.w / 2) (t.y + t.h / 2) ox oy x2 y2 < max t.w t.h / 2 + 2) (targets w)
            in
            List.foldl (\t acc -> damageTarget t.id 20 (cos a * 24) p.idx False acc) w hits
                |> (\acc -> { acc | beams = { x1 = ox, y1 = oy, x2 = x2, y2 = y2, life = 0.12, color = "#00e8d8" } :: acc.beams })
                |> sfx "laser"



-- CAMERA -----------------------------------------------------------------------


updateCamera : Float -> World -> World
updateCamera dt w =
    let
        lvl =
            w.level
    in
    case w.phase of
        BossFight ->
            arenaView dt w

        Cleared _ ->
            arenaView dt w

        AllKO _ ->
            arenaView dt w

        _ ->
            -- Climb, Gather and BossIntro follow the group
            let
                ps =
                    alivePlayers w

                n =
                    toFloat (List.length ps)
            in
            if n == 0 then
                w

            else
                let
                    avg =
                        List.sum (List.map pcy ps) / n

                    leader =
                        List.minimum (List.map .y ps) |> Maybe.withDefault 0

                    lowest =
                        List.maximum (List.map (\p -> p.y + 15) ps) |> Maybe.withDefault 0

                    target =
                        min (avg - gameH * 0.55) (leader - 60)
                            |> max (lowest + 8 - gameH)

                    cam =
                        if target < w.camY then
                            w.camY + (target - w.camY) * min 1 (dt * 3)

                        else
                            w.camY
                in
                { w | camY = max lvl.arenaCamTop cam }


{-| Glide the camera to frame the whole boss arena.
-}
arenaView : Float -> World -> World
arenaView dt w =
    let
        top =
            w.level.arenaCamTop

        cam =
            w.camY + (top - w.camY) * min 1 (dt * 4)
    in
    { w
        | camY =
            if abs (cam - top) < 0.5 then
                top

            else
                cam
    }


{-| Players who fall off the bottom of the screen get put back near the group.
-}
handleFallen : World -> World
handleFallen w =
    List.foldl
        (\p acc ->
            if p.hp > 0 && p.y > acc.camY + gameH + 16 then
                let
                    ps =
                        List.filter (\q -> q.hp > 0 && q.y <= acc.camY + gameH) (alivePlayers acc)

                    avgX =
                        if List.isEmpty ps then
                            gameW / 2

                        else
                            List.sum (List.map pcx ps) / toFloat (List.length ps)

                    spots =
                        acc.level.platforms
                            |> List.filter (\pl -> not pl.gone && pl.y > acc.camY + 50 && pl.y < acc.camY + gameH - 20)
                            |> List.sortBy (\pl -> abs (pl.x + pl.w / 2 - avgX) + abs (pl.y - (acc.camY + gameH * 0.6)) * 0.5)

                    ( nx, ny ) =
                        case spots of
                            pl :: _ ->
                                ( pl.x + pl.w / 2 - 5, pl.y - 16 )

                            [] ->
                                ( gameW / 2, acc.camY + 40 )
                in
                acc
                    |> mapPlayer p.idx (\q -> { q | x = nx, y = ny, vx = 0, vy = 0, hp = max 1 (q.hp - 10), invuln = 1.5 })
                    |> sfx "hurt"
                    |> floatText (nx + 5) (ny - 6) "OOPS! -10" "#fc7460"
                    |> burst (nx + 5) (ny + 8) "#fcfcfc" 10 60 2

            else
                acc
        )
        w
        w.players



-- ENEMIES ----------------------------------------------------------------------


updateEnemies : Float -> World -> World
updateEnemies dt w =
    List.foldl (\e acc -> updateEnemy dt e.id acc) w w.enemies


setEnemy : Enemy -> World -> World
setEnemy e w =
    { w
        | enemies =
            List.map
                (\x ->
                    if x.id == e.id then
                        e

                    else
                        x
                )
                w.enemies
    }


updateEnemy : Float -> Int -> World -> World
updateEnemy dt id w =
    case List.head (List.filter (\x -> x.id == id) w.enemies) of
        Nothing ->
            w

        Just e0 ->
            if e0.dead then
                w

            else
                let
                    e =
                        { e0
                            | t = e0.t + dt
                            , flash = e0.flash - dt
                            , active = e0.y < w.camY + gameH + 48 && e0.y + e0.h > w.camY - 64
                        }
                in
                if not e.active then
                    setEnemy e w

                else
                    let
                        tgt =
                            nearestPlayer (e.x + e.w / 2) (e.y + e.h / 2) w

                        ( e2, w2 ) =
                            case e.kind of
                                BabyDragon ->
                                    flyer dt tgt e w

                                _ ->
                                    walker dt tgt e w

                        w3 =
                            setEnemy e2 w2
                    in
                    if e2.dead then
                        w3

                    else
                        List.foldl
                            (\p acc ->
                                if overlaps (playerBox p) e2 then
                                    hurtPlayer p.idx (8 * dmgMul e2.lvl) (Just (e2.x + e2.w / 2)) acc

                                else
                                    acc
                            )
                            w3
                            (alivePlayers w3)


dmgMul : Int -> Float
dmgMul lvl =
    0.75 + 0.05 * toFloat lvl


walker : Float -> Maybe Player -> Enemy -> World -> ( Enemy, World )
walker dt tgt e w =
    case platformById e.plat w of
        Nothing ->
            ( e, w )

        Just pl ->
            if pl.gone then
                -- the platform crumbled away: fall off screen
                let
                    vy =
                        e.vy + grav * dt
                in
                ( { e | vy = vy, y = e.y + vy * dt, dead = e.y > w.camY + gameH + 60 }, w )

            else
                let
                    vx =
                        e.vx * (0.02 ^ dt)

                    cx =
                        e.x + e.w / 2

                    shot =
                        Make.proj
                in
                case e.kind of
                    Goon ->
                        let
                            speed =
                                (50 + 6 * toFloat e.lvl) * 0.4

                            chasing =
                                case tgt of
                                    Just p ->
                                        abs (p.y + 15 - (e.y + e.h)) < 6 && abs (pcx p - cx) < 104

                                    Nothing ->
                                        False

                            facing0 =
                                case ( chasing, tgt ) of
                                    ( True, Just p ) ->
                                        if pcx p < cx then
                                            -1

                                        else
                                            1

                                    _ ->
                                        e.facing

                            sp =
                                if chasing then
                                    speed * 1.7

                                else
                                    speed

                            x1 =
                                e.x + pl.dx + (facing0 * sp + vx) * dt

                            ( x2, facing ) =
                                if x1 < pl.x then
                                    ( pl.x, 1 )

                                else if x1 + e.w > pl.x + pl.w then
                                    ( pl.x + pl.w - e.w, -1 )

                                else
                                    ( x1, facing0 )

                            -- later goons toss coins
                            ( shootT, w2 ) =
                                case tgt of
                                    Just p ->
                                        if e.lvl >= 5 && e.shootT - dt <= 0 && abs (pcx p - cx) < 160 then
                                            ( 2.5 + rnd (w.tick + e.id) * 1.5
                                            , spawnProj { shot | kind = CoinP, x = cx, y = e.y + 3, vx = sign (pcx p - cx) * 120, vy = -140, grav = 360, r = 3, dmg = 8 * dmgMul e.lvl, life = 2.5 } w
                                            )

                                        else
                                            ( e.shootT - dt, w )

                                    Nothing ->
                                        ( e.shootT - dt, w )
                        in
                        ( { e | x = x2, facing = facing, vx = vx, y = pl.y - e.h, shootT = shootT }, w2 )

                    _ ->
                        let
                            facing =
                                case tgt of
                                    Just p ->
                                        if pcx p < cx then
                                            -1

                                        else
                                            1

                                    Nothing ->
                                        e.facing

                            x2 =
                                clamp pl.x (pl.x + pl.w - e.w) (e.x + pl.dx + vx * dt)

                            ( shootT, w2 ) =
                                case tgt of
                                    Just p ->
                                        if e.shootT - dt <= 0 && abs (pcy p - (e.y + e.h / 2)) < 68 && abs (pcx p - cx) < 216 then
                                            let
                                                a =
                                                    atan2 (pcy p - (e.y + 6)) (pcx p - cx)
                                            in
                                            ( max 1.2 (2.2 + rnd (w.tick + e.id) - toFloat e.lvl * 0.06)
                                            , spawnProj { shot | kind = Arrow, x = cx, y = e.y + 6, vx = cos a * 172, vy = sin a * 172, r = 2.5, dmg = 9 * dmgMul e.lvl, life = 2 } w
                                                |> sfx "shoot"
                                            )

                                        else
                                            ( e.shootT - dt, w )

                                    Nothing ->
                                        ( e.shootT - dt, w )
                        in
                        ( { e | x = x2, facing = facing, vx = vx, y = pl.y - e.h, shootT = shootT }, w2 )


flyer : Float -> Maybe Player -> Enemy -> World -> ( Enemy, World )
flyer dt tgt e w =
    let
        cx =
            e.x + e.w / 2

        cy =
            e.y + e.h / 2

        near =
            case tgt of
                Just p ->
                    if sqrt ((pcx p - cx) ^ 2 + (pcy p - cy) ^ 2) < 184 then
                        Just p

                    else
                        Nothing

                Nothing ->
                    Nothing

        ( tx, ty, facing ) =
            case near of
                Just p ->
                    ( pcx p
                        + (if cx < pcx p then
                            -52

                           else
                            52
                          )
                        - e.w
                        / 2
                    , p.y - 36 + sin (e.t * 3) * 8
                    , if cx < pcx p then
                        1

                      else
                        -1
                    )

                Nothing ->
                    let
                        hx =
                            e.homeX + sin (e.t * 0.8) * 24
                    in
                    ( hx
                    , e.homeY + sin (e.t * 1.7) * 10
                    , if hx > e.x then
                        1

                      else
                        -1
                    )

        dx =
            tx - e.x

        dy =
            ty - e.y

        d =
            max 0.001 (sqrt (dx * dx + dy * dy))

        sp =
            min ((70 + 6 * toFloat e.lvl) * 0.4) (d * 3)

        ( shootT, w2 ) =
            case near of
                Just p ->
                    if e.shootT - dt <= 0 then
                        let
                            a =
                                atan2 (pcy p - cy) (pcx p - cx)

                            shot =
                                Make.proj
                        in
                        ( max 1.2 (2.2 + rnd (w.tick + e.id) - toFloat e.lvl * 0.1)
                        , spawnProj { shot | kind = Fireball, x = cx + facing * 5, y = cy, vx = cos a * 96, vy = sin a * 96, r = 3, dmg = 8 * dmgMul e.lvl, life = 3 } w
                            |> sfx "shoot"
                        )

                    else
                        ( e.shootT - dt, w )

                Nothing ->
                    ( e.shootT, w )
    in
    ( { e
        | x = clamp 0 (gameW - e.w) (e.x + dx / d * sp * dt + e.vx * dt)
        , y = e.y + dy / d * sp * dt
        , vx = e.vx * (0.02 ^ dt)
        , facing = facing
        , shootT = shootT
      }
    , w2
    )



-- PROJECTILES ------------------------------------------------------------------


updateProjs : Float -> World -> World
updateProjs dt w =
    let
        moved =
            List.map
                (\p ->
                    let
                        vy =
                            p.vy + p.grav * dt
                    in
                    { p | life = p.life - dt, vy = vy, x = p.x + p.vx * dt, y = p.y + vy * dt }
                )
                w.projs
    in
    List.foldl (projStep dt) { w | projs = [] } moved
        |> (\acc -> { acc | projs = List.reverse acc.projs })


keep : Proj -> World -> World
keep p w =
    { w | projs = p :: w.projs }


projStep : Float -> Proj -> World -> World
projStep dt p w =
    let
        offscreen =
            p.y > w.camY + gameH + 60 || p.y < w.camY - 120 || p.x < -24 || p.x > gameW + 24

        prevY =
            p.y - p.vy * dt

        landedOn =
            p.vy > 0 && List.any (\pl -> not pl.gone && p.x > pl.x && p.x < pl.x + pl.w && prevY + p.r <= pl.y + 1 && p.y + p.r >= pl.y) w.level.platforms
    in
    if offscreen then
        w

    else
        case p.owner of
            ByPlayer pi ->
                let
                    hit =
                        targets w
                            |> List.filter (\t -> not (List.member t.id p.hits) && circleBox p.x p.y p.r t)
                in
                case p.kind of
                    FartP ->
                        if landedOn || p.life <= 0 || not (List.isEmpty hit) then
                            w
                                |> makeZone p.x (p.y - 8) 38 3 28 False
                                |> sfx "fart"
                                |> burst p.x p.y "#58d854" 12 60 3

                        else
                            keep p w

                    Rocket ->
                        if p.life <= 0 || not (List.isEmpty hit) then
                            explode p.x p.y p.explode p.dmg pi w

                        else
                            let
                                w2 =
                                    if modBy 2 w.tick == 0 then
                                        particle { x = p.x - sign p.vx * 4, y = p.y, vx = 0, vy = -8, life = 0.4, maxLife = 0.4, size = 2.5, grav = 0, color = "#7c7c84", kind = Puff } w

                                    else
                                        w
                            in
                            keep p w2

                    _ ->
                        if p.life <= 0 then
                            w

                        else
                            case hit of
                                [] ->
                                    keep p w

                                _ ->
                                    let
                                        w2 =
                                            List.foldl (\t acc -> damageTarget t.id p.dmg (sign p.vx * 32) pi False acc)
                                                w
                                                (if p.pierce then
                                                    hit

                                                 else
                                                    List.take 1 hit
                                                )
                                    in
                                    if p.pierce then
                                        keep { p | hits = List.map .id hit ++ p.hits } w2

                                    else
                                        burst p.x p.y "#fcfc58" 4 60 1.5 w2

            Hostile ->
                let
                    hitPlayers =
                        alivePlayers w
                            |> List.filter (\pl -> not (List.member pl.idx p.hits) && circleBox p.x p.y p.r (playerBox pl))

                    applyHit pl acc =
                        acc
                            |> hurtPlayer pl.idx p.dmg (Just p.x)
                            |> (if p.slow > 0 then
                                    mapPlayer pl.idx (\q -> { q | slow = max q.slow p.slow })

                                else
                                    identity
                               )
                in
                if p.life <= 0 then
                    w

                else if not (List.isEmpty hitPlayers) then
                    let
                        w2 =
                            List.foldl applyHit w hitPlayers
                    in
                    if p.pierce then
                        keep { p | hits = List.map .idx hitPlayers ++ p.hits } w2

                    else
                        burst p.x p.y "#fcfcfc" 5 60 1.5 w2

                else if p.floorBreak && landedOn then
                    -- thrown gold becomes loot you can grab!
                    case p.kind of
                        CoinP ->
                            pickup Coin (p.x - 4) (p.y - 8) 0 -60 6 w

                        DiamondP ->
                            pickup Diamond (p.x - 4) (p.y - 8) 0 -60 6 w

                        -- stolen crypto lands as gold you can grab back
                        BitcoinP ->
                            pickup Coin (p.x - 4) (p.y - 8) 0 -60 6 w

                        _ ->
                            burst p.x p.y (projColor p.kind) 8 80 2 w

                else
                    keep p w


projColor : ProjKind -> String
projColor k =
    case k of
        Fireball ->
            "#fc7c00"

        Snowball ->
            "#fcfcfc"

        Icicle ->
            "#c4f0fc"

        _ ->
            "#7c7c84"



-- ZONES, ITEMS, PARTICLES --------------------------------------------------------


updateZones : Float -> World -> World
updateZones dt w =
    let
        zs =
            List.filter (\z -> z.life > 0) (List.map (\z -> { z | life = z.life - dt, t = z.t + dt }) w.zones)

        -- overlapping poison puddles only hurt once: use the strongest one
        poisonPlayers acc =
            List.foldl
                (\p a2 ->
                    let
                        dps =
                            zs
                                |> List.filter (\z -> z.hostile && sqrt ((pcx p - z.x) ^ 2 + (pcy p - z.y) ^ 2) < z.r)
                                |> List.map .dps
                                |> List.maximum
                    in
                    case dps of
                        Just d ->
                            a2 |> drainPlayer p.idx (d * dt) |> mapPlayer p.idx (\q -> { q | poison = max q.poison 1 })

                        Nothing ->
                            a2
                )
                acc
                (alivePlayers acc)

        apply z acc =
            if z.hostile then
                acc

            else
                List.foldl
                    (\t a2 ->
                        let
                            tx =
                                clamp t.x (t.x + t.w) z.x

                            ty =
                                clamp t.y (t.y + t.h) z.y
                        in
                        if sqrt ((tx - z.x) ^ 2 + (ty - z.y) ^ 2) < z.r then
                            damageTarget t.id (z.dps * dt) 0 -1 True a2

                        else
                            a2
                    )
                    acc
                    (targets acc)

        puff z acc =
            let
                s =
                    acc.tick * 23 + round (z.x * 7)
            in
            if rnd s < 0.4 then
                particle
                    { x = z.x + rndRange (s + 1) -z.r z.r * 0.8
                    , y = z.y + rndRange (s + 2) (-z.r * 0.6) (z.r * 0.4)
                    , vx = 0
                    , vy = -10
                    , life = 0.8
                    , maxLife = 0.8
                    , size = rndRange (s + 3) 3 7
                    , grav = 0
                    , color =
                        if z.hostile then
                            "#00a800"

                        else
                            "#b8f818"
                    , kind = Puff
                    }
                    acc

            else
                acc
    in
    List.foldl (\z acc -> acc |> apply z |> puff z) (poisonPlayers { w | zones = zs }) zs


updateItems : Float -> World -> World
updateItems dt w =
    let
        plats =
            List.filter (\pl -> not pl.gone) w.level.platforms

        move it =
            let
                t =
                    it.t + dt
            in
            if not it.phys then
                { it | t = t }

            else
                let
                    vy =
                        min maxFall (it.vy + 560 * dt)

                    x =
                        clamp 0 (gameW - 8) (it.x + it.vx * dt)

                    y =
                        it.y + vy * dt

                    prevB =
                        it.y + 8
                in
                case List.head (List.filter (\pl -> x + 8 > pl.x && x < pl.x + pl.w && prevB <= pl.y + 1 && y + 8 >= pl.y) plats) of
                    Just pl ->
                        { it
                            | t = t
                            , x = x
                            , y = pl.y - 8
                            , vy =
                                if vy > 48 then
                                    -vy * 0.35

                                else
                                    0
                            , vx = it.vx * 0.7
                        }

                    Nothing ->
                        { it | t = t, x = x, y = y, vy = vy, vx = it.vx * (0.3 ^ dt) }

        expired it =
            it.gone || (it.life > 0 && it.t > it.life) || it.y > w.camY + gameH + 100

        collect it acc =
            let
                taker =
                    alivePlayers acc
                        |> List.filter (\p -> overlaps (playerBox p) (itemBox it))
                        |> List.head
            in
            case ( it.kind, taker ) of
                ( Coin, Just p ) ->
                    ( acc |> mapPlayer p.idx (\q -> { q | gold = q.gold + 1 }) |> sfx "coin", True )

                ( Diamond, Just p ) ->
                    ( acc |> mapPlayer p.idx (\q -> { q | diamonds = q.diamonds + 1 }) |> sfx "diamond" |> floatText (pcx p) (p.y - 6) "DIAMOND!" "#00e8d8", True )

                ( Heart, Just p ) ->
                    if p.hp < 100 then
                        ( acc |> mapPlayer p.idx (\q -> { q | hp = min 100 (q.hp + 25) }) |> sfx "heart" |> floatText (pcx p) (p.y - 6) "+25" "#fc7460", True )

                    else
                        ( acc, False )

                _ ->
                    ( acc, False )

        step1 it ( acc, kept ) =
            if expired it then
                ( acc, kept )

            else
                let
                    ( acc2, taken ) =
                        collect it acc
                in
                if taken then
                    ( acc2, kept )

                else
                    ( acc2, it :: kept )

        ( w2, items ) =
            List.foldl step1 ( w, [] ) (List.map move w.items)
    in
    { w2 | items = List.reverse items }


updateParticles : Float -> World -> World
updateParticles dt w =
    { w
        | particles =
            w.particles
                |> List.filterMap
                    (\p ->
                        if p.life - dt <= 0 then
                            Nothing

                        else
                            let
                                vy =
                                    p.vy + p.grav * dt
                            in
                            Just { p | life = p.life - dt, x = p.x + p.vx * dt, y = p.y + vy * dt, vy = vy }
                    )
                |> List.take 450
        , beams = List.filter (\b -> b.life > 0) (List.map (\b -> { b | life = b.life - dt }) w.beams)
    }



-- HAZARDS & PHASES ---------------------------------------------------------------


hazards : Float -> World -> World
hazards dt w =
    let
        -- falling-rock warnings tick down and turn into rocks
        ( fire, waiting ) =
            List.partition (\wr -> wr.t <= 0) (List.map (\wr -> { wr | t = wr.t - dt }) w.warnings)

        shot =
            Make.proj

        w1 =
            List.foldl
                (\wr acc ->
                    spawnProj
                        { shot
                            | kind =
                                if w.level.theme.deco == Circuit then
                                    Spark

                                else
                                    Rock
                            , x = wr.x
                            , y = acc.camY - 8
                            , vy = 40
                            , grav = 280
                            , r = 5
                            , dmg = 12 * dmgMul w.level.n
                            , life = 5
                            , floorBreak = True
                        }
                        acc
                )
                { w | warnings = waiting }
                fire

        w2 =
            if w1.level.hazards && w1.phase == Climb then
                if w1.hazardT - dt <= 0 then
                    { w1
                        | hazardT = max 1.8 (3.5 - 0.15 * toFloat w1.level.n)
                        , warnings = { x = rndRange (w1.tick * 5) 12 (gameW - 12), t = 0.9 } :: w1.warnings
                    }

                else
                    { w1 | hazardT = w1.hazardT - dt }

            else
                w1
    in
    case w2.phase of
        BossFight ->
            if w2.heartT - dt <= 0 then
                pickup Heart (rndRange (w2.tick * 3) 20 (gameW - 28)) (w2.camY + 26) 0 0 20 { w2 | heartT = 14 }

            else
                { w2 | heartT = w2.heartT - dt }

        _ ->
            w2


arenaId : World -> Int
arenaId w =
    List.filter (\p -> p.kind == Arena) w.level.platforms |> List.head |> Maybe.map .id |> Maybe.withDefault -1


phaseLogic : Float -> World -> World
phaseLogic dt w =
    let
        ps =
            alivePlayers w

        banner =
            w.banner
                |> Maybe.andThen
                    (\b ->
                        if b.t - dt <= 0 then
                            Nothing

                        else
                            Just { b | t = b.t - dt }
                    )

        w1 =
            { w | banner = banner }

        isCleared =
            case w.phase of
                Cleared _ ->
                    True

                AllKO _ ->
                    True

                _ ->
                    False
    in
    if List.isEmpty ps && not isCleared then
        { w1 | phase = AllKO 0, banner = Just { text = "OH NO!", sub = "EVERYONE IS KNOCKED OUT", t = 3, color = "#fc3c3c" } }
            |> sfx "ko"

    else
        case w1.phase of
            Climb ->
                if List.any (\p -> p.standing == arenaId w1) ps then
                    { w1 | phase = Gather 0, banner = Just { text = "BOSS PLATFORM!", sub = "EVERYBODY CLIMB UP!", t = 2, color = "#fcd000" } }

                else
                    w1

            Gather t ->
                let
                    aid =
                        arenaId w1

                    allOn =
                        List.all (\p -> p.standing == aid || p.y + 15 <= w1.level.arenaY) ps
                in
                if allOn || t > 4 then
                    let
                        bossName =
                            Maybe.map (\b -> b.def.name) w1.boss |> Maybe.withDefault "BOSS"

                        bossSub =
                            Maybe.map (\b -> b.def.sub) w1.boss |> Maybe.withDefault ""

                        straggler p =
                            if p.hp > 0 && p.standing /= aid && p.y + 15 > w1.level.arenaY then
                                { p | x = 20 + toFloat p.idx * 16, y = w1.level.arenaY - 15, vy = 0, invuln = 1 }

                            else
                                p
                    in
                    { w1 | phase = BossFight, players = List.map straggler w1.players, banner = Just { text = bossName, sub = bossSub, t = 2.5, color = "#fc3c3c" } }
                        |> Boss.wake

                else
                    { w1 | phase = Gather (t + dt) }

            BossFight ->
                case w1.boss of
                    Just b ->
                        if b.hp <= 0 && b.state == Fighting then
                            bossDefeated b w1

                        else
                            w1

                    Nothing ->
                        w1

            BossIntro t ->
                { w1 | phase = BossIntro (t + dt) }

            Cleared t ->
                if t + dt > 6 then
                    { w1 | outcome = LevelWon }

                else
                    { w1 | phase = Cleared (t + dt) }

            AllKO t ->
                if t + dt > 2.5 then
                    { w1 | outcome = GameLost }

                else
                    { w1 | phase = AllKO (t + dt) }


bossDefeated : Boss -> World -> World
bossDefeated b w =
    let
        cx =
            b.x + b.w / 2

        cy =
            b.y + b.h / 2

        loot kind count acc0 =
            List.foldl
                (\i acc ->
                    let
                        s =
                            acc.tick * 41 + i * 13 + count
                    in
                    pickup kind (cx - 4) cy (rndRange s -120 120) (rndRange (s + 1) -300 -150) 0 acc
                )
                acc0
                (List.range 0 (count - 1))

        isDragon =
            b.n >= 11

        killAll acc =
            List.foldl
                (\e a2 ->
                    if e.dead then
                        a2

                    else
                        burst (e.x + e.w / 2) (e.y + e.h / 2) "#fcfcfc" 8 80 2 a2
                )
                { acc | enemies = List.map (\e -> { e | dead = True }) acc.enemies }
                acc.enemies
    in
    { w
        | boss = Just { b | state = Defeated, beams = [], breath = Nothing, atk = Nothing }
        , phase = Cleared 0
        , projs = List.filter (\p -> p.owner /= Hostile) w.projs
        , zones = List.filter (\z -> not z.hostile) w.zones
        , banner =
            Just
                (if isDragon then
                    { text = "DIABLO IS DEFEATED!", sub = "THE GOLD IS FREE!", t = 5, color = "#fcd000" }

                 else
                    { text = "LEVEL CLEAR!", sub = "GRAB THE LOOT!", t = 4, color = "#fcd000" }
                )
    }
        |> killAll
        |> sfx "boom"
        |> sfx "clear"
        |> (\acc -> List.foldl (\p a -> sfx (Aura.voice p) a) acc (alivePlayers acc))
        |> shake 10
        |> burst cx cy "#fc7c00" 30 160 4
        |> burst cx cy "#fcfc58" 20 120 3
        |> burst cx cy "#fcfcfc" 20 200 2
        |> loot Coin
            (if isDragon then
                60

             else
                24
            )
        |> loot Diamond
            (if isDragon then
                16

             else
                5
            )
        |> loot Heart 3


cleanup : World -> World
cleanup w =
    { w | enemies = List.filter (\e -> not e.dead) w.enemies }


{-| Debug helper: jump everyone straight to the boss arena.
-}
skipToBoss : World -> World
skipToBoss w =
    { w
        | players = List.map (\p -> { p | y = w.level.arenaY - 15, x = 30 + toFloat p.idx * 20, vy = 0 }) w.players
        , camY = w.level.arenaCamTop
        , phase = Gather 10
    }


{-| The world y of the invisible floor along the bottom edge of the screen.
-}
screenFloor : World -> Float
screenFloor w =
    w.camY + gameH - 4
