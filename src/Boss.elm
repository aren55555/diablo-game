module Boss exposing (eyePoint, mouthPoint, update, wake)

{-| Boss behaviour: idle movement, a telegraphed attack pattern, beams
and breath. Each boss picks attacks from its definition in Data.
-}

import Fx exposing (..)
import Make
import Types exposing (..)
import Util exposing (..)


bcx : Boss -> Float
bcx b =
    b.x + b.w / 2


bcy : Boss -> Float
bcy b =
    b.y + b.h / 2


{-| Local offset (facing right, from bottom-center) to world position.
-}
point : Boss -> ( Float, Float ) -> ( Float, Float )
point b ( ox, oy ) =
    ( bcx b + ox * b.facing, b.y + b.h + oy )


mouthPoint : Boss -> ( Float, Float )
mouthPoint b =
    point b b.def.mouth


eyePoint : Boss -> ( Float, Float ) -> ( Float, Float )
eyePoint =
    point


wake : World -> World
wake w =
    case w.boss of
        Just b ->
            { w | boss = Just { b | state = Fighting, quote = Maybe.withDefault "" (List.head b.def.quotes), quoteT = 2.5, idleT = 1.6 } }
                |> sfx "roar"
                |> shake 5

        Nothing ->
            w


fast : Boss -> Float
fast b =
    if b.enraged then
        1.25

    else
        1


projStats : ProjKind -> ( Float, Float, Float )
projStats kind =
    -- radius, damage, slow
    case kind of
        Spear ->
            ( 3, 12, 0 )

        Fireball ->
            ( 4, 12, 0 )

        Rock ->
            ( 5, 14, 0 )

        Sonic ->
            ( 5, 10, 0 )

        MoneyBag ->
            ( 5, 14, 0 )

        CoinP ->
            ( 3.5, 10, 0 )

        Snowball ->
            ( 4.5, 10, 1.6 )

        Icicle ->
            ( 4, 12, 1.2 )

        Arrow ->
            ( 2.5, 10, 0 )

        Bone ->
            ( 4, 10, 0 )

        DiamondP ->
            ( 4, 12, 0 )

        _ ->
            ( 3, 10, 0 )


hostile : ProjKind -> Float -> Float -> Float -> Float -> Float -> Proj
hostile kind x y vx vy dmgMul =
    let
        ( r, dmg, slow ) =
            projStats kind

        p =
            Make.proj
    in
    { p | kind = kind, x = x, y = y, vx = vx, vy = vy, r = r, dmg = dmg * dmgMul, slow = slow, life = 4 }


update : Float -> World -> World
update dt w =
    case w.boss of
        Nothing ->
            w

        Just b0 ->
            let
                b =
                    { b0 | t = b0.t + dt, flash = b0.flash - dt, tele = max 0 (b0.tele - dt), quoteT = b0.quoteT - dt }
            in
            if b.state /= Fighting then
                { w | boss = Just b }

            else
                case nearestPlayer (bcx b) (bcy b) w of
                    Nothing ->
                        { w | boss = Just { b | beams = [], breath = Nothing } }

                    Just tgt ->
                        let
                            bq =
                                if b.quoteT < -4 && not (List.isEmpty b.def.quotes) then
                                    let
                                        qi =
                                            modBy (List.length b.def.quotes) (b.quoteIdx + 1)
                                    in
                                    { b | quoteIdx = qi, quote = Maybe.withDefault "" (listGet qi b.def.quotes), quoteT = 2 }

                                else
                                    b

                            ( b2, w2 ) =
                                case bq.atk of
                                    Nothing ->
                                        idle dt tgt bq w

                                    Just atk ->
                                        runAttack dt tgt atk bq w

                            b3 =
                                physics dt b2

                            w3 =
                                List.foldl
                                    (\p acc ->
                                        if overlaps (playerBox p) b3 then
                                            hurtPlayer p.idx (10 * b3.dmgMul) (Just (bcx b3)) acc

                                        else
                                            acc
                                    )
                                    w2
                                    (alivePlayers w2)
                        in
                        { w3 | boss = Just b3 }
                            |> beams dt b3
                            |> breath dt b3


physics : Float -> Boss -> Boss
physics dt b =
    let
        clampX bb =
            { bb | x = clamp 2 (gameW - bb.w - 2) bb.x }
    in
    if b.def.fly then
        clampX b

    else
        let
            vy =
                min 570 (b.vy + 800 * dt)

            y =
                b.y + vy * dt

            x =
                b.x + b.vx * dt
        in
        if y + b.h >= b.floorY then
            clampX { b | x = x, y = b.floorY - b.h, vy = 0, onGround = True, landed = b.landed || (not b.onGround && vy > 120) }

        else
            clampX { b | x = x, y = y, vy = vy, onGround = False }


idle : Float -> Player -> Boss -> World -> ( Boss, World )
idle dt tgt b w =
    let
        sp =
            b.def.speed
                * (if b.enraged then
                    1.3

                   else
                    1
                  )

        facing =
            if bcx b < pcx tgt then
                1

            else
                -1

        moved =
            if b.def.fly then
                let
                    side =
                        if bcx b < pcx tgt then
                            -1

                        else
                            1

                    tx =
                        clamp 4 (gameW - b.w - 4) (pcx tgt + side * 92 - b.w / 2)

                    ty =
                        b.floorY - b.def.hover - b.h + sin (b.t * 2) * 11
                in
                { b | x = b.x + clamp (-sp * dt) (sp * dt) (tx - b.x), y = b.y + clamp (-80 * dt) (80 * dt) (ty - b.y), facing = facing }

            else
                { b
                    | facing = facing
                    , vx =
                        if abs (pcx tgt - bcx b) > b.w / 2 + 28 then
                            facing * sp

                        else
                            0
                }

        idleT =
            moved.idleT - dt
    in
    if idleT <= 0 then
        startAttack { moved | idleT = 0 } w

    else
        ( { moved | idleT = idleT }, w )


startAttack : Boss -> World -> ( Boss, World )
startAttack b w =
    let
        n =
            List.length b.def.attacks

        i0 =
            floor (rnd (w.tick * 31 + b.n * 7) * toFloat n)

        i =
            if i0 == b.lastAtk && n > 1 then
                modBy n (i0 + 1)

            else
                i0

        spec =
            Maybe.withDefault Charge (listGet i b.def.attacks)
    in
    ( { b | atk = Just { spec = spec, specIdx = i, tt = 0, stage = 0, fired = 0, tx = 0, ty = 0 }, lastAtk = i, vx = 0 }, w )


endAttack : Boss -> Boss
endAttack b =
    let
        base =
            0.9 + 0.7 * rnd (round (b.t * 1000))

        mul =
            if b.enraged then
                0.6

            else
                1
    in
    { b | atk = Nothing, idleT = max 0.35 (base * mul * (1.15 - 0.035 * toFloat b.n)) }


runAttack : Float -> Player -> Attack -> Boss -> World -> ( Boss, World )
runAttack dt tgt atk0 b w =
    let
        a =
            { atk0 | tt = atk0.tt + dt }

        f =
            fast b

        set bb aa =
            { bb | atk = Just aa }

        face =
            if bcx b < pcx tgt then
                1

            else
                -1
    in
    case a.spec of
        Charge ->
            if a.stage == 0 then
                if a.tt > 0.7 / f then
                    ( set { b | vx = 0, facing = face } { a | stage = 1, tt = 0 }, sfx "roar" w )

                else
                    ( set { b | tele = 0.1, vx = 0, facing = face } a, w )

            else
                let
                    vx =
                        b.facing * (184 + 9 * toFloat b.n) * f

                    w2 =
                        if modBy 3 w.tick == 0 then
                            particle { x = bcx b - b.facing * b.w / 2, y = b.floorY - 2, vx = -vx * 0.2, vy = -24, life = 0.4, maxLife = 0.4, size = 3, grav = 0, color = "#c4b49c", kind = Puff } w

                        else
                            w
                in
                if b.x <= 3 || b.x >= gameW - b.w - 3 || a.tt > 2.2 then
                    ( endAttack { b | vx = 0 }, w2 |> shake 5 |> sfx "stomp" )

                else
                    ( set { b | vx = vx } a, w2 )

        Swoop ->
            case a.stage of
                0 ->
                    if a.tt > 0.6 / f then
                        ( set b { a | stage = 1, tt = 0 }, sfx "roar" w )

                    else
                        ( set { b | tele = 0.1, facing = face } { a | tx = pcx tgt, ty = b.floorY - b.h * 0.55 }, w )

                1 ->
                    let
                        dx =
                            a.tx - bcx b

                        dy =
                            a.ty - bcy b

                        d =
                            sqrt (dx * dx + dy * dy)

                        sp =
                            248 * f
                    in
                    if d < sp * dt || a.tt > 1.6 then
                        ( set b { a | stage = 2, tt = 0 }, w )

                    else
                        ( set { b | x = b.x + dx / d * sp * dt, y = b.y + dy / d * sp * dt } a, w )

                _ ->
                    let
                        ty =
                            b.floorY - b.def.hover - b.h

                        y =
                            b.y - 128 * dt
                    in
                    if y <= ty then
                        ( endAttack { b | y = ty }, w )

                    else
                        ( set { b | y = y, x = b.x + b.facing * 64 * dt } a, w )

        Shoot kind n spread speed grav ->
            let
                volleys =
                    if b.enraged then
                        2

                    else
                        1

                bt =
                    { b | tele = 0.1 }
            in
            if a.tt > 0.5 / f + toFloat a.fired * 0.4 && a.fired < volleys then
                let
                    ( mx, my ) =
                        mouthPoint b

                    base =
                        atan2 (pcy tgt - my) (pcx tgt - mx)

                    shot i acc =
                        let
                            off =
                                if n > 1 then
                                    (toFloat i / toFloat (n - 1) - 0.5) * 2 * spread

                                else
                                    0

                            p =
                                if grav then
                                    let
                                        hp =
                                            hostile kind mx my (clamp -speed speed (pcx tgt - mx) + off * 104) (-168 + rndRange (acc.tick + i) -24 24) b.dmgMul
                                    in
                                    { hp | grav = 320, floorBreak = True }

                                else
                                    hostile kind mx my (cos (base + off) * speed) (sin (base + off) * speed) b.dmgMul
                        in
                        spawnProj p acc
                in
                ( set bt { a | fired = a.fired + 1 }, List.foldl shot (sfx "shoot" w) (List.range 0 (n - 1)) )

            else if a.tt > 0.5 / f + toFloat volleys * 0.4 + 0.2 then
                ( endAttack b, w )

            else
                ( set bt a, w )

        Radial kind n speed ->
            let
                rings =
                    if b.enraged then
                        2

                    else
                        1

                bt =
                    { b | tele = 0.1 }
            in
            if a.tt > 0.6 / f + toFloat a.fired * 0.5 && a.fired < rings then
                let
                    shot i acc =
                        let
                            ang =
                                toFloat i / toFloat n * 2 * pi + toFloat a.fired * (pi / toFloat n)
                        in
                        spawnProj (hostile kind (bcx b) (bcy b) (cos ang * speed) (sin ang * speed) b.dmgMul) acc
                in
                ( set bt { a | fired = a.fired + 1 }, List.foldl shot (sfx "shoot" w) (List.range 0 (n - 1)) )

            else if a.tt > 0.6 / f + toFloat rings * 0.5 + 0.2 then
                ( endAttack b, w )

            else
                ( set bt a, w )

        Stomp ->
            if b.def.fly then
                ( endAttack b, w )

            else if a.stage == 0 then
                if a.tt > 0.4 / f then
                    ( set { b | vy = -352, vx = clamp -168 168 ((pcx tgt - bcx b) / 0.85), onGround = False, landed = False } { a | stage = 1, tt = 0 }, w )

                else
                    ( set { b | tele = 0.1, vx = 0 } a, w )

            else if a.tt > 0.1 && b.landed then
                let
                    wave dir =
                        let
                            p =
                                Make.proj
                        in
                        spawnProj { p | kind = Wave, x = bcx b + dir * b.w / 2, y = b.floorY - 6, vx = dir * 160 * f, r = 7, dmg = 14 * b.dmgMul, life = 2.6, pierce = True }
                in
                ( endAttack { b | vx = 0, landed = False }
                , w |> wave -1 |> wave 1 |> shake 6 |> sfx "stomp" |> burst (bcx b) b.floorY "#c4b49c" 18 120 3
                )

            else if a.tt > 3 then
                ( endAttack b, w )

            else
                ( set b a, w )

        Rain kind n ->
            if a.tt < 0.3 then
                ( set { b | tele = 0.1 } a, w )

            else
                let
                    dur =
                        1.6

                    want =
                        min n (floor ((a.tt - 0.3) / dur * toFloat n) + 1)

                    ps =
                        alivePlayers w

                    drop i acc =
                        let
                            s =
                                acc.tick * 13 + i * 101 + b.n

                            x =
                                if rnd s < 0.45 then
                                    case listGet (floor (rnd (s + 1) * toFloat (List.length ps))) ps of
                                        Just p ->
                                            clamp 8 (gameW - 8) (pcx p + rndRange (s + 2) -24 24)

                                        Nothing ->
                                            rndRange (s + 3) 8 (gameW - 8)

                                else
                                    rndRange (s + 3) 8 (gameW - 8)

                            ( r, _, _ ) =
                                projStats kind

                            p0 =
                                hostile kind x (acc.camY - 12) 0 48 b.dmgMul
                        in
                        spawnProj { p0 | grav = 280, r = r + 1, floorBreak = True, shadow = True } acc

                    w2 =
                        List.foldl drop w (List.range a.fired (want - 1))

                    a2 =
                        { a | fired = max a.fired want }
                in
                if a.tt > 0.3 + dur + 0.3 then
                    ( endAttack b, w2 )

                else
                    ( set b a2, w2 )

        Summon kind n ->
            if a.tt > 0.6 && a.fired == 0 then
                let
                    alive =
                        List.length (List.filter (\e -> not e.dead) w.enemies)

                    count =
                        min n (max 0 (5 - alive))

                    arenaId =
                        List.filter (\p -> p.kind == Arena) w.level.platforms |> List.head |> Maybe.map .id |> Maybe.withDefault -1

                    spawn i acc =
                        let
                            left =
                                modBy 2 i == 0

                            s =
                                acc.tick * 7 + i * 3
                        in
                        case kind of
                            BabyDragon ->
                                let
                                    acc2 =
                                        spawnEnemy BabyDragon
                                            (if left then
                                                8

                                             else
                                                gameW - 24
                                            )
                                            (acc.camY + 24)
                                            -1
                                            b.n
                                            acc
                                in
                                { acc2
                                    | enemies =
                                        case acc2.enemies of
                                            e :: rest ->
                                                { e | homeX = rndRange s 40 (gameW - 56), homeY = b.floorY - rndRange (s + 1) 64 104 } :: rest

                                            [] ->
                                                []
                                }

                            _ ->
                                let
                                    acc2 =
                                        spawnEnemy kind
                                            (if left then
                                                8

                                             else
                                                gameW - 20
                                            )
                                            b.floorY
                                            arenaId
                                            b.n
                                            acc
                                in
                                { acc2
                                    | enemies =
                                        case acc2.enemies of
                                            e :: rest ->
                                                { e
                                                    | facing =
                                                        if left then
                                                            1

                                                        else
                                                            -1
                                                }
                                                    :: rest

                                            [] ->
                                                []
                                }
                in
                ( set { b | tele = 0.1 } { a | fired = 1 }, List.foldl spawn (sfx "roar" w) (List.range 0 (count - 1)) )

            else if a.tt > 1.1 then
                ( endAttack b, w )

            else
                ( set { b | tele = 0.1 } a, w )

        LaserA _ ->
            case a.stage of
                0 ->
                    let
                        dir =
                            if rnd (w.tick * 3 + 1) < 0.5 then
                                -1

                            else
                                1

                        eyes =
                            if List.isEmpty b.def.eyes then
                                [ ( 0, -b.h * 0.7 ) ]

                            else
                                b.def.eyes

                        mk i eye =
                            let
                                d =
                                    if modBy 2 i == 0 then
                                        dir

                                    else
                                        -dir

                                sx =
                                    clamp 0 gameW (pcx tgt - d * 104)
                            in
                            { eye = eye, sx = sx, ex = clamp 0 gameW (pcx tgt + d * 152), px = sx, on = False }
                    in
                    ( set { b | vx = 0, beams = List.indexedMap mk eyes, facing = face } { a | stage = 1, tt = 0 }, sfx "zap" w )

                1 ->
                    if a.tt > 0.9 / f then
                        ( set { b | beams = List.map (\bm -> { bm | on = True }) b.beams } { a | stage = 2, tt = 0 }, w )

                    else
                        ( set { b | tele = 0.1, vx = 0 } a, w )

                _ ->
                    let
                        dur =
                            1.7 / f

                        k =
                            min 1 (a.tt / dur)
                    in
                    if a.tt > dur then
                        ( endAttack { b | beams = [] }, w )

                    else
                        ( set { b | beams = List.map (\bm -> { bm | px = lerp bm.sx bm.ex k }) b.beams } a, w )

        BreathA kind ->
            let
                ( mx, my ) =
                    mouthPoint b

                want =
                    atan2 (pcy tgt - my) (pcx tgt - mx)
            in
            if a.stage == 0 then
                if a.tt > 0.6 / f then
                    ( set { b | vx = 0, breath = Just { kind = kind, ang = want, t = 0, dropT = 0 } } { a | stage = 1, tt = 0 }, sfx "breath" w )

                else
                    ( set { b | tele = 0.1, vx = 0, facing = face } a, w )

            else
                let
                    dur =
                        if b.enraged then
                            2.4

                        else
                            1.8
                in
                if a.tt > dur then
                    ( endAttack { b | breath = Nothing }, w )

                else
                    case b.breath of
                        Just br ->
                            let
                                turn =
                                    clamp (-0.9 * dt * f) (0.9 * dt * f) (angleDiff want br.ang)
                            in
                            ( set { b | vx = 0, breath = Just { br | ang = br.ang + turn } } a, w )

                        Nothing ->
                            ( endAttack b, w )


beams : Float -> Boss -> World -> World
beams dt b w =
    List.foldl
        (\bm acc ->
            if not bm.on then
                acc

            else
                let
                    ( ex, ey ) =
                        eyePoint b bm.eye

                    hitPlayers =
                        List.filter (\p -> pointSegDist (pcx p) (pcy p) ex ey bm.px b.floorY < 10) (alivePlayers acc)

                    acc2 =
                        List.foldl (\p a2 -> drainPlayer p.idx (28 * b.dmgMul * dt) a2) acc hitPlayers

                    s =
                        acc.tick * 17 + round bm.px
                in
                particle { x = bm.px, y = b.floorY - 1, vx = rndRange s -48 48, vy = rndRange (s + 1) -88 -24, life = 0.35, maxLife = 0.35, size = 1.5, grav = 240, color = "#fc3c3c", kind = Dot } acc2
        )
        w
        b.beams


breath : Float -> Boss -> World -> World
breath dt b w =
    case b.breath of
        Nothing ->
            w

        Just br ->
            let
                ( mx, my ) =
                    mouthPoint b

                len =
                    160

                half =
                    0.33

                colors =
                    case br.kind of
                        Fire ->
                            [ "#fcfc58", "#fc7c00", "#d82800" ]

                        Ice ->
                            [ "#fcfcfc", "#c4f0fc", "#3cbcfc" ]

                        Poison ->
                            [ "#b8f818", "#58d854", "#00a800" ]

                puff i acc =
                    let
                        s =
                            acc.tick * 19 + i * 7

                        ang =
                            br.ang + rndRange s -half half * 0.9

                        sp =
                            rndRange (s + 1) 128 208
                    in
                    particle
                        { x = mx
                        , y = my
                        , vx = cos ang * sp
                        , vy = sin ang * sp
                        , life = len / sp
                        , maxLife = len / sp
                        , size = rndRange (s + 2) 2 4.5
                        , grav = 0
                        , color = Maybe.withDefault "#fff" (listGet (modBy 3 i) colors)
                        , kind = Puff
                        }
                        acc

                w1 =
                    List.foldl puff w (List.range 0 3)

                inCone p =
                    let
                        dx =
                            pcx p - mx

                        dy =
                            pcy p - my
                    in
                    sqrt (dx * dx + dy * dy) < len && abs (angleDiff (atan2 dy dx) br.ang) < half

                hit p acc =
                    case br.kind of
                        Fire ->
                            drainPlayer p.idx (16 * b.dmgMul * dt) acc

                        Ice ->
                            acc
                                |> drainPlayer p.idx (7 * b.dmgMul * dt)
                                |> (if p.slow <= 0 then
                                        sfx "freeze"

                                    else
                                        identity
                                   )
                                |> mapPlayer p.idx (\q -> { q | slow = 1.8 })

                        Poison ->
                            acc
                                |> drainPlayer p.idx (4 * b.dmgMul * dt)
                                |> mapPlayer p.idx (\q -> { q | poison = 3 })

                w2 =
                    List.foldl hit w1 (List.filter inCone (alivePlayers w1))

                dropT =
                    br.dropT - dt

                -- poison breath leaves puddles where it meets the floor
                ( w3, dropT2 ) =
                    if br.kind == Poison && dropT <= 0 then
                        let
                            s =
                                sin br.ang

                            dist =
                                if s > 0.1 then
                                    min len ((b.floorY - my) / s)

                                else
                                    len
                        in
                        ( makeZone (mx + cos br.ang * dist) (b.floorY - 8) 22 4 (6 * b.dmgMul) True w2, 0.35 )

                    else
                        ( w2, dropT )
            in
            { w3 | boss = Maybe.map (\bb -> { bb | breath = Just { br | t = br.t + dt, dropT = dropT2 } }) w3.boss }
