module Pixel exposing
    ( Canvas
    , dot
    , ellipse
    , hline
    , line
    , new
    , outline
    , poly
    , recolor
    , rect
    , thickLine
    , toRows
    )

{-| A tiny raster DSL used to author the 8-bit sprites in code.
Pixels are palette characters; '.' means transparent.
-}

import Dict exposing (Dict)


type alias Canvas =
    { w : Int, h : Int, px : Dict ( Int, Int ) Char }


new : Int -> Int -> Canvas
new w h =
    { w = w, h = h, px = Dict.empty }


dot : Int -> Int -> Char -> Canvas -> Canvas
dot x y c cv =
    if x >= 0 && y >= 0 && x < cv.w && y < cv.h then
        { cv | px = Dict.insert ( x, y ) c cv.px }

    else
        cv


rect : Int -> Int -> Int -> Int -> Char -> Canvas -> Canvas
rect x y w h c cv =
    List.foldl
        (\j acc -> List.foldl (\i acc2 -> dot (x + i) (y + j) c acc2) acc (List.range 0 (w - 1)))
        cv
        (List.range 0 (h - 1))


hline : Int -> Int -> Int -> Char -> Canvas -> Canvas
hline x y w c =
    rect x y w 1 c


{-| Filled ellipse; a pixel is filled when its center lies inside.
-}
ellipse : Float -> Float -> Float -> Float -> Char -> Canvas -> Canvas
ellipse cx cy rx ry c cv =
    let
        x0 =
            floor (cx - rx)

        x1 =
            ceiling (cx + rx)

        y0 =
            floor (cy - ry)

        y1 =
            ceiling (cy + ry)

        inside i j =
            ((toFloat i + 0.5 - cx) / rx) ^ 2 + ((toFloat j + 0.5 - cy) / ry) ^ 2 <= 1
    in
    List.foldl
        (\j acc ->
            List.foldl
                (\i acc2 ->
                    if inside i j then
                        dot i j c acc2

                    else
                        acc2
                )
                acc
                (List.range x0 x1)
        )
        cv
        (List.range y0 y1)


line : Int -> Int -> Int -> Int -> Char -> Canvas -> Canvas
line x0 y0 x1 y1 c cv =
    let
        steps =
            max (abs (x1 - x0)) (abs (y1 - y0))

        pt k =
            if steps == 0 then
                ( x0, y0 )

            else
                ( round (toFloat x0 + toFloat (x1 - x0) * toFloat k / toFloat steps)
                , round (toFloat y0 + toFloat (y1 - y0) * toFloat k / toFloat steps)
                )
    in
    List.foldl
        (\k acc ->
            let
                ( px, py ) =
                    pt k
            in
            dot px py c acc
        )
        cv
        (List.range 0 steps)


thickLine : Int -> Int -> Int -> Int -> Int -> Char -> Canvas -> Canvas
thickLine x0 y0 x1 y1 t c cv =
    List.foldl
        (\o acc -> List.foldl (\o2 acc2 -> line (x0 + o) (y0 + o2) (x1 + o) (y1 + o2) c acc2) acc (List.range 0 (t - 1)))
        cv
        (List.range 0 (t - 1))


{-| Filled polygon (even-odd rule on pixel centers).
-}
poly : List ( Float, Float ) -> Char -> Canvas -> Canvas
poly pts c cv =
    let
        xs =
            List.map Tuple.first pts

        ys =
            List.map Tuple.second pts

        edges =
            List.map2 Tuple.pair pts (List.drop 1 pts ++ List.take 1 pts)

        inside px py =
            List.foldl
                (\( ( ax, ay ), ( bx, by ) ) acc ->
                    if (ay > py) /= (by > py) && px < (bx - ax) * (py - ay) / (by - ay) + ax then
                        not acc

                    else
                        acc
                )
                False
                edges

        rng a b =
            List.range (floor (Maybe.withDefault 0 a)) (ceiling (Maybe.withDefault 0 b))
    in
    List.foldl
        (\j acc ->
            List.foldl
                (\i acc2 ->
                    if inside (toFloat i + 0.5) (toFloat j + 0.5) then
                        dot i j c acc2

                    else
                        acc2
                )
                acc
                (rng (List.minimum xs) (List.maximum xs))
        )
        cv
        (rng (List.minimum ys) (List.maximum ys))


{-| Add a 1px outline (4-neighbour) around every drawn pixel.
-}
outline : Char -> Canvas -> Canvas
outline c cv =
    let
        filled i j =
            Dict.member ( i, j ) cv.px

        edge i j =
            not (filled i j) && (filled (i - 1) j || filled (i + 1) j || filled i (j - 1) || filled i (j + 1))

        newPx =
            List.concatMap
                (\j ->
                    List.filterMap
                        (\i ->
                            if edge i j then
                                Just ( i, j )

                            else
                                Nothing
                        )
                        (List.range 0 (cv.w - 1))
                )
                (List.range 0 (cv.h - 1))
    in
    List.foldl (\( i, j ) acc -> dot i j c acc) cv newPx


recolor : Char -> Char -> Canvas -> Canvas
recolor from to cv =
    { cv
        | px =
            Dict.map
                (\_ v ->
                    if v == from then
                        to

                    else
                        v
                )
                cv.px
    }


toRows : Canvas -> List String
toRows cv =
    List.map
        (\j ->
            String.fromList
                (List.map (\i -> Maybe.withDefault '.' (Dict.get ( i, j ) cv.px)) (List.range 0 (cv.w - 1)))
        )
        (List.range 0 (cv.h - 1))
