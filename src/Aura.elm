module Aura exposing (update, voice)

{-| Each character leaves its own aura behind (sparkles, music notes, paw
prints, cobwebs...) and has a signature voice played when attacking.
-}

import Data
import Fx exposing (findPlayer, mapPlayer, pcx, sfx)
import Types exposing (..)
import Util exposing (rnd, rndRange)


voice : Player -> String
voice p =
    "voice_" ++ (Data.characterInfo p.character).key


{-| Tick the player's aura and voice timers. attackPressed = attack key was
just pressed this frame.
-}
update : Float -> Bool -> Int -> World -> World
update dt attackPressed idx w =
    case findPlayer idx w of
        Nothing ->
            w

        Just p ->
            if p.hp <= 0 then
                w

            else
                let
                    speak =
                        attackPressed && p.voiceT <= 0

                    w1 =
                        if speak then
                            sfx (voice p) w

                        else
                            w

                    voiceT =
                        if speak then
                            2.5

                        else
                            p.voiceT - dt

                    auraT =
                        p.auraT - dt
                in
                if auraT > 0 then
                    mapPlayer idx (\q -> { q | auraT = auraT, voiceT = voiceT }) w1

                else
                    let
                        ( interval, parts ) =
                            spawn p (w.tick * 13 + idx * 7919)
                    in
                    { w1 | particles = parts ++ w1.particles }
                        |> mapPlayer idx (\q -> { q | auraT = interval, voiceT = voiceT })


img : String -> Bool -> Float -> Float -> Float -> Float -> Float -> Float -> Particle
img id flip x y vx vy life grav =
    { x = x, y = y, vx = vx, vy = vy, life = life, maxLife = life, size = 1, grav = grav, color = "", kind = Img id flip }


dot : String -> Float -> Float -> Float -> Float -> Float -> Float -> Particle
dot color x y vx vy life size =
    { x = x, y = y, vx = vx, vy = vy, life = life, maxLife = life, size = size, grav = 0, color = color, kind = Dot }


{-| How long until the next aura particle, and what to spawn now.
-}
spawn : Player -> Int -> ( Float, List Particle )
spawn p s =
    let
        r i =
            rnd (s + i)

        rr i a b =
            rndRange (s + i) a b

        cx =
            pcx p

        feet =
            p.y + 15

        moving =
            abs p.vx > 20 || not p.onGround

        walking =
            p.onGround && abs p.vx > 20

        flip =
            p.facing < 0

        pickOf i a b =
            if r i < 0.5 then
                a

            else
                b
    in
    case p.character of
        Goblin ->
            ( if moving then
                0.08

              else
                0.3
            , [ dot (pickOf 0 "#b8f818" "#58d854") (cx + rr 1 -4 4) (feet - rr 2 0 5) (rr 3 -6 6) -20 0.5 1 ]
            )

        Fairy ->
            ( if moving then
                0.06

              else
                0.15
            , [ img (pickOf 0 "a_star" "a_star2") False (cx + rr 1 -7 7) (p.y + rr 2 0 15) (rr 3 -10 10) 10 0.7 0 ]
            )

        PopStar ->
            ( if moving then
                0.3

              else
                0.55
            , [ img (pickOf 0 "a_note" "a_note2") False (cx + rr 1 -6 6) p.y (rr 3 -12 12) -22 1.2 0
              , dot "#fcd000" (cx + rr 4 -5 5) (p.y + rr 5 2 14) 0 6 0.4 1
              , dot "#f878f8" (cx + rr 6 -5 5) (p.y + rr 7 2 14) 0 6 0.4 1
              ]
            )

        Dino ->
            if walking then
                ( 0.2, [ img "a_footprint" flip (cx - p.facing * 3) (feet - 1) 0 0 2.5 0 ] )

            else
                ( 0.2, [] )

        TreeMan ->
            ( 0.25, [ img (pickOf 0 "a_leaf" "a_leaf2") (r 4 < 0.5) (cx + rr 1 -6 6) (p.y + 2) (rr 3 -15 15) 18 1.6 0 ] )

        PieWoman ->
            if walking then
                ( 0.3, [ img "a_splat" flip cx (feet - 1) 0 0 2.2 0 ] )

            else
                ( 0.4, [ img "a_crumb" False (cx + p.facing * 5) (p.y + 9) (rr 1 -10 10) 0 0.8 200 ] )

        BlackBerry ->
            if walking then
                ( 0.16, [ img "a_paw" False (cx - p.facing * 4 + rr 1 -1 1) (feet - 1) 0 0 2.5 0 ] )

            else if p.onGround then
                ( 0.9, [ img "a_heart" False (cx + rr 1 -4 4) (p.y + 2) 0 -18 1 0 ] )

            else
                ( 0.2, [] )

        BlockMan ->
            ( if moving then
                0.18

              else
                0.6
            , [ img "a_block" False (cx + rr 1 -4 4) (p.y + 6) (rr 2 -30 30) -60 0.8 300 ]
            )

        PoopyDiaper ->
            ( if moving then
                0.22

              else
                0.35
            , [ img "a_stink" (r 4 < 0.5) (cx + rr 1 -5 5) (p.y + 6) 0 -14 1.1 0
              , img "a_fly" False (cx + rr 2 -8 8) (p.y + rr 3 0 12) (rr 5 -25 25) (rr 6 -10 10) 0.6 0
              ]
            )

        Ghosty ->
            if moving then
                -- spooky afterimages
                ( 0.05, [ img (Data.spriteFor p) flip cx (feet - 7) 0 0 0.35 0 ] )

            else
                ( 0.3, [ { x = cx + rr 1 -5 5, y = feet - 2, vx = 0, vy = -6, life = 0.8, maxLife = 0.8, size = 3, grav = 0, color = "#c4f0fc", kind = Puff } ] )

        BeerMan ->
            ( if moving then
                0.14

              else
                0.3
            , [ img "a_bubble" False (cx + rr 1 -5 5) (p.y + 2) (rr 2 -6 6) -24 1 0 ]
            )

        Spider ->
            if walking then
                ( 0.32, [ img "a_web" False cx (feet - 3) 0 0 5 0 ] )

            else if not p.onGround then
                ( 0.04, [ dot "#fcfcfc" cx (p.y + 7) 0 0 0.6 1 ] )

            else
                ( 0.3, [] )
