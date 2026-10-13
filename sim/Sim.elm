port module Sim exposing (main)

{-| Headless smoke test: bot players climb a level and fight its boss.
Reports what happened so we can check every level is beatable.
Used by `just sim`.
-}

import Data
import Dict
import Json.Encode as E
import Make
import Types exposing (..)
import Util exposing (rnd)
import World


port report : E.Value -> Cmd msg


type alias Flags =
    { level : Int, seconds : Int, players : Int, weapon : String, bossOnly : Bool, arrive : Bool, fall : Bool }


main : Program Flags () ()
main =
    Platform.worker
        { init = \flags -> ( (), report (run flags) )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


weaponOf : String -> Weapon
weaponOf s =
    case s of
        "nunchucks" ->
            Nunchucks

        "sword" ->
            Sword

        "machinegun" ->
            MachineGun

        "bazooka" ->
            Bazooka

        "fartbomb" ->
            FartBomb

        "laser" ->
            Laser

        _ ->
            Fists


{-| A simple climbing/fighting bot.
-}
bot : World -> Int -> Player -> Pad
bot w t p =
    let
        feet =
            p.y + 15

        cx =
            p.x + 5

        jitter =
            rnd (t // 20 + p.idx * 1000) < 0.15

        ( targetX, wantJump ) =
            case w.phase of
                Climb ->
                    let
                        above =
                            w.level.platforms
                                |> List.filter (\pl -> not pl.gone && pl.y < feet - 10 && pl.y > feet - 95)
                                |> List.sortBy (\pl -> abs (pl.x + pl.w / 2 - cx) + (feet - pl.y) * 0.3)
                                |> List.head
                    in
                    case above of
                        Just pl ->
                            let
                                tx =
                                    clamp (pl.x + 6) (pl.x + pl.w - 16) cx
                            in
                            ( tx, abs (tx - cx) < 40 || not p.onGround )

                        Nothing ->
                            ( cx + 60, True )

                _ ->
                    case w.boss of
                        Just b ->
                            let
                                bcx =
                                    b.x + b.w / 2

                                melee =
                                    p.weapon == Fists || p.weapon == Nunchucks || p.weapon == Sword

                                side =
                                    if cx < bcx then
                                        -1

                                    else
                                        1

                                keepAway =
                                    if melee then
                                        b.w / 2 + 6

                                    else
                                        b.w / 2 + 90
                            in
                            ( clamp 12 360 (bcx + side * keepAway), modBy 90 (t + p.idx * 20) < 10 )

                        Nothing ->
                            ( cx, False )

        dx =
            targetX - cx

        jumpNow =
            wantJump && (p.onGround || (p.vy > -40 && p.airJumps > 0))
    in
    { left = dx < -3 || (jitter && dx < 0)
    , right = dx > 3 || (jitter && dx > 0)
    , up = jumpNow || p.vy < 0
    , down = True
    , upP = jumpNow
    , downP = modBy 30 t == 0
    , leftP = False
    , rightP = False
    }


run : Flags -> E.Value
run flags =
    let
        players =
            List.map
                (\i ->
                    let
                        p =
                            Make.player i (Data.characterAt (i * 3))
                    in
                    { p | weapon = weaponOf flags.weapon }
                )
                (List.range 0 (flags.players - 1))

        fresh =
            World.new flags.level players

        start =
            if flags.bossOnly then
                World.skipToBoss fresh

            else if flags.fall then
                -- players drop from mid-screen with the ground far below the camera
                { fresh
                    | players = List.map (\p -> { p | y = -560, x = 4 + toFloat p.idx * 120, vy = 0, invuln = 0 }) fresh.players
                    , camY = -700
                    , enemies = []
                }

            else if flags.arrive then
                -- everyone has just climbed onto the boss platform (still in the Climb
                -- phase, camera where the climb camera would have it); they stand still
                { fresh
                    | players = List.map (\p -> { p | y = fresh.level.arenaY - 15, x = 30 + toFloat p.idx * 20, vy = 0 }) fresh.players
                    , camY = fresh.level.arenaY - 140
                }

            else
                fresh

        steps =
            flags.seconds * 60

        loop t w bossT dmg =
            if t >= steps || w.outcome /= Ongoing then
                ( ( t, w, bossT ), dmg )

            else
                let
                    w2 =
                        World.step (1 / 60)
                            (\i ->
                                if flags.arrive || flags.fall then
                                    noPad

                                else
                                    List.filter (\p -> p.idx == i) w.players |> List.head |> Maybe.map (bot w t) |> Maybe.withDefault noPad
                            )
                            w

                    bossT2 =
                        if bossT < 0 && (w2.phase == BossFight || isGather w2.phase) then
                            t

                        else
                            bossT

                    lost =
                        List.sum (List.map2 (\a b -> max 0 (a.hp - b.hp)) w.players w2.players)

                    key =
                        w.boss |> Maybe.andThen .atk |> Maybe.map (.spec >> specName) |> Maybe.withDefault "idle"

                    dmg2 =
                        if lost > 0 then
                            Dict.update key (\v -> Just (lost + Maybe.withDefault 0 v)) dmg

                        else
                            dmg
                in
                loop (t + 1) w2 bossT2 dmg2

        ( ( ticks, final, bossStart ), damage ) =
            loop 0 start -1 Dict.empty
    in
    E.object
        [ ( "level", E.int flags.level )
        , ( "seconds", E.float (toFloat ticks / 60) )
        , ( "bossReachedAt", E.float (toFloat bossStart / 60) )
        , ( "phase", E.string (phaseName final.phase) )
        , ( "outcome", E.string (outcomeName final.outcome) )
        , ( "climbed", E.float (-(final.camY + 210) / -(final.level.arenaCamTop + 210)) )
        , ( "bossHp", E.float (final.boss |> Maybe.map (\b -> b.hp / b.maxHp) |> Maybe.withDefault 0) )
        , ( "hp", E.list (\p -> E.int (round p.hp)) final.players )
        , ( "gold", E.list (\p -> E.int p.gold) final.players )
        , ( "reachable", E.bool (reachable start.level) )
        , ( "feetAboveArena", E.list (\p -> E.int (round (final.level.arenaY - (p.y + 15)))) final.players )
        , ( "enemies", E.int (List.length final.enemies) )
        , ( "arenaGap", E.float (start.level.platforms |> List.filter (\p -> p.kind == Normal) |> List.map (\p -> p.y - start.level.arenaY) |> List.minimum |> Maybe.withDefault 0) )
        , ( "onScreen", E.bool (List.all (\p -> p.y + 15 <= final.camY + 240) final.players) )
        , ( "damageBy", E.dict identity (\v -> E.int (round v)) damage )
        ]


noPad : Pad
noPad =
    { left = False, right = False, up = False, down = False, upP = False, downP = False, leftP = False, rightP = False }


phaseName : Phase -> String
phaseName ph =
    case ph of
        Climb ->
            "climb"

        Gather _ ->
            "gather"

        BossIntro _ ->
            "intro"

        BossFight ->
            "boss"

        Cleared _ ->
            "cleared"

        AllKO _ ->
            "allko"


outcomeName : Outcome -> String
outcomeName o =
    case o of
        Ongoing ->
            "ongoing"

        LevelWon ->
            "won"

        GameLost ->
            "lost"


{-| Can the arena be reached from the ground, assuming a double jump gains
~95px and drifts ~80px sideways? (Moving platforms use their center.)
-}
reachable : Level -> Bool
reachable lvl =
    let
        canHop a b =
            let
                dy =
                    a.y - b.y

                gap =
                    max 0 (max (b.x - (a.x + a.w)) (a.x - (b.x + b.w)))
            in
            dy <= 92 && gap <= 70

        expand seen frontier =
            case frontier of
                [] ->
                    seen

                a :: rest ->
                    let
                        next =
                            List.filter (\b -> not (List.member b.id seen) && canHop a b) lvl.platforms
                    in
                    expand (List.map .id next ++ seen) (rest ++ next)

        start =
            List.filter (\p -> p.kind == Ground) lvl.platforms

        seenIds =
            expand (List.map .id start) start
    in
    List.any (\p -> p.kind == Arena && List.member p.id seenIds) lvl.platforms


isGather : Phase -> Bool
isGather ph =
    case ph of
        Gather _ ->
            True

        _ ->
            False


specName : AttackSpec -> String
specName s =
    case s of
        Charge ->
            "charge"

        Swoop ->
            "swoop"

        Shoot _ _ _ _ _ ->
            "shoot"

        Radial _ _ _ ->
            "radial"

        Stomp ->
            "stomp"

        Rain _ _ ->
            "rain"

        Summon _ _ ->
            "summon"

        LaserA _ ->
            "laser"

        BreathA Fire ->
            "fire"

        BreathA Ice ->
            "ice"

        BreathA Poison ->
            "poison"

        ThrowCash _ ->
            "cash"

        Jet _ ->
            "jet"
