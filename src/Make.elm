module Make exposing (boss, enemy, item, player, proj)

{-| Constructors for game entities.
-}

import Types exposing (..)
import Util exposing (gameW)


player : Int -> Character -> Player
player idx character =
    { idx = idx
    , character = character
    , x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , facing = 1
    , onGround = False
    , standing = -1
    , coyote = 0
    , jumpBuf = 0
    , airJumps = 1
    , cd = 0
    , atkT = 0
    , invuln = 1
    , slow = 0
    , poison = 0
    , hurtFx = 0
    , hp = 100
    , weapon = Fists
    , gold = 0
    , diamonds = 0
    , kills = 0
    , anim = 0
    , ghostT = 0
    , prompt = 0
    , auraT = 0
    , voiceT = 0
    }


{-| Enemy standing on a platform (pass the platform's top y and id), or
flying (plat = -1).
-}
enemy : Int -> EnemyKind -> Float -> Float -> Int -> Int -> Enemy
enemy id kind x y plat lvl =
    let
        s =
            1 + 0.12 * toFloat (lvl - 1)

        ( ( w, h ), hp ) =
            case kind of
                Goon ->
                    ( ( 11, 15 ), 30 * s )

                BabyDragon ->
                    ( ( 14, 10 ), 22 * s )

                Archer ->
                    ( ( 10, 16 ), 26 * s )

                Bodyguard ->
                    ( ( 11, 16 ), 55 * s )

        y0 =
            if plat >= 0 then
                y - h

            else
                y
    in
    { id = id
    , kind = kind
    , x = x
    , y = y0
    , w = w
    , h = h
    , vx = 0
    , vy = 0
    , facing = -1
    , t = toFloat (modBy 100 (id * 37)) / 10
    , flash = 0
    , hp = hp
    , maxHp = hp
    , plat = plat
    , homeX = x
    , homeY = y0
    , shootT = 1.5 + toFloat (modBy 10 id) / 5
    , lvl = lvl
    , dead = False
    , active = False
    }


item : Int -> ItemKind -> Float -> Float -> Item
item id kind x y =
    { id = id, kind = kind, x = x, y = y, vx = 0, vy = 0, phys = False, t = toFloat (modBy 7 id), life = 0, gone = False }


proj : Proj
proj =
    { id = 0
    , kind = Bullet
    , x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , r = 3
    , dmg = 10
    , life = 3
    , grav = 0
    , owner = Hostile
    , pierce = False
    , hits = []
    , slow = 0
    , floorBreak = False
    , shadow = False
    , explode = 0
    }


boss : Int -> BossDef -> Float -> Int -> Boss
boss n def arenaY numPlayers =
    let
        hp =
            toFloat (round (def.hp * (0.55 + 0.3 * toFloat numPlayers)))
    in
    { def = def
    , n = n
    , x = gameW - def.w - 36
    , y =
        if def.fly then
            arenaY - def.hover - def.h

        else
            arenaY - def.h
    , w = def.w
    , h = def.h
    , vx = 0
    , vy = 0
    , facing = -1
    , t = 0
    , flash = 0
    , hp = hp
    , maxHp = hp
    , state = Sleeping
    , atk = Nothing
    , idleT = 2
    , lastAtk = -1
    , enraged = False
    , tele = 0
    , quote = ""
    , quoteT = 0
    , quoteIdx = 0
    , onGround = True
    , landed = False
    , beams = []
    , breath = Nothing
    , floorY = arenaY
    , dmgMul = 0.75 + 0.05 * toFloat n
    }
