module Util exposing
    ( Box
    , angleDiff
    , circleBox
    , gameH
    , gameW
    , hash
    , lerp
    , listGet
    , overlaps
    , pointSegDist
    , rnd
    , rndRange
    , sign
    )

{-| Screen size, math helpers and a tiny deterministic hash-based RNG.
-}

import Bitwise


{-| Logical resolution. The canvas is upscaled with crisp pixels.
-}
gameW : Float
gameW =
    384


gameH : Float
gameH =
    240


type alias Box a =
    { a | x : Float, y : Float, w : Float, h : Float }


lerp : Float -> Float -> Float -> Float
lerp a b t =
    a + (b - a) * t


sign : Float -> Float
sign v =
    if v < 0 then
        -1

    else if v > 0 then
        1

    else
        0


overlaps : Box a -> Box b -> Bool
overlaps a b =
    a.x < b.x + b.w && a.x + a.w > b.x && a.y < b.y + b.h && a.y + a.h > b.y


circleBox : Float -> Float -> Float -> Box a -> Bool
circleBox cx cy r b =
    let
        nx =
            clamp b.x (b.x + b.w) cx

        ny =
            clamp b.y (b.y + b.h) cy
    in
    (cx - nx) ^ 2 + (cy - ny) ^ 2 < r * r


angleDiff : Float -> Float -> Float
angleDiff a b =
    let
        d =
            a - b

        wrapped =
            d - 2 * pi * toFloat (floor ((d + pi) / (2 * pi)))
    in
    wrapped


pointSegDist : Float -> Float -> Float -> Float -> Float -> Float -> Float
pointSegDist px py ax ay bx by =
    let
        dx =
            bx - ax

        dy =
            by - ay

        l2 =
            dx * dx + dy * dy

        t =
            if l2 == 0 then
                0

            else
                clamp 0 1 (((px - ax) * dx + (py - ay) * dy) / l2)
    in
    sqrt ((px - (ax + dx * t)) ^ 2 + (py - (ay + dy * t)) ^ 2)


listGet : Int -> List a -> Maybe a
listGet i xs =
    List.head (List.drop i xs)



-- RNG ----------------------------------------------------------------------


{-| 32-bit integer multiply (like Math.imul) without losing precision.
-}
mul32 : Int -> Int -> Int
mul32 a b =
    let
        ah =
            Bitwise.and (Bitwise.shiftRightZfBy 16 a) 0xFFFF

        al =
            Bitwise.and a 0xFFFF
    in
    Bitwise.or 0 (al * b + Bitwise.shiftLeftBy 16 (Bitwise.or 0 (ah * b)))


hash : Int -> Int
hash n =
    let
        a =
            Bitwise.xor n (Bitwise.shiftRightZfBy 16 n)

        b =
            mul32 a 0x045D9F3B

        c =
            Bitwise.xor b (Bitwise.shiftRightZfBy 16 b)

        d =
            mul32 c 0x045D9F3B
    in
    Bitwise.xor d (Bitwise.shiftRightZfBy 16 d)


{-| Deterministic pseudo-random float in [0, 1) for a given seed.
-}
rnd : Int -> Float
rnd seed =
    toFloat (Bitwise.shiftRightZfBy 0 (hash (seed * 747796405 + 2891336453))) / 4294967296


rndRange : Int -> Float -> Float -> Float
rndRange seed a b =
    a + rnd seed * (b - a)
