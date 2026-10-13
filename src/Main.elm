module Main exposing (main)

{-| Mount Diablo: a 4-player local co-op climbing game.

Elm owns all game state and logic. The TypeScript platform layer draws the
commands we send each frame, plays sounds and reports gamepads.

-}

import Browser
import Browser.Events as Events
import Data
import Html
import Json.Decode as D
import Json.Encode as E
import Make
import Ports
import Render
import Set exposing (Set)
import Sprites
import Types exposing (..)
import Util exposing (listGet, rnd)
import World


type alias Flags =
    { debug : Bool, titleBoss : Int }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = \_ -> Html.text ""
        }



-- MODEL ------------------------------------------------------------------------


type Screen
    = Title
    | Select
    | Story
    | Playing World Bool
    | Interlude Int String (List Player)
    | GameOver World
    | Victory (List Player)


type alias Held =
    { left : Bool, right : Bool, up : Bool, down : Bool }


type alias RawPad =
    { left : Bool, right : Bool, up : Bool, down : Bool, start : Bool }


type alias Model =
    { screen : Screen
    , screenT : Float
    , time : Float
    , keys : Set String
    , tapped : Set String
    , prevHeld : List Held
    , pads : List RawPad
    , prevStart : Bool
    , joined : List Bool
    , muted : Bool
    , music : String
    , tauntIdx : Int
    , startLevel : Int
    , titleBoss : Int
    , debug : Bool
    , picks : List Int
    , ready : List Bool
    , readyT : Float
    }


type Msg
    = Tick Float
    | KeyDown String Bool
    | KeyUp String
    | Gamepads D.Value
    | ClearKeys


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { screen = Title
      , screenT = 0
      , time = 0
      , keys = Set.empty
      , tapped = Set.empty
      , prevHeld = List.repeat 4 noHeld
      , pads = []
      , prevStart = False
      , joined = [ False, False, False, False ]
      , muted = False
      , music = ""
      , tauntIdx = 0
      , startLevel = 1
      , titleBoss = clamp 1 11 flags.titleBoss
      , debug = flags.debug
      , picks = [ 0, 3, 6, 9 ]
      , ready = [ False, False, False, False ]
      , readyT = 0
      }
    , Ports.sprites encodeSprites
    )


noHeld : Held
noHeld =
    { left = False, right = False, up = False, down = False }


encodeSprites : E.Value
encodeSprites =
    E.object
        [ ( "palette", E.object (List.map (\( c, hex ) -> ( String.fromChar c, E.string hex )) Sprites.palette) )
        , ( "sprites"
          , E.list
                (\s ->
                    E.object
                        [ ( "id", E.string s.id )
                        , ( "rows", E.list E.string s.rows )
                        , ( "pal", E.object (List.map (\( c, hex ) -> ( String.fromChar c, E.string hex )) s.pal) )
                        ]
                )
                Sprites.all
          )
        ]



-- INPUT ------------------------------------------------------------------------


{-| up = jump, down = attack / grab weapon.
-}
keyMap : Int -> { left : List String, right : List String, up : List String, down : List String }
keyMap i =
    case i of
        0 ->
            { left = [ "KeyA" ], right = [ "KeyD" ], up = [ "KeyW" ], down = [ "KeyS" ] }

        1 ->
            { left = [ "ArrowLeft" ], right = [ "ArrowRight" ], up = [ "ArrowUp" ], down = [ "ArrowDown" ] }

        2 ->
            { left = [ "KeyF" ], right = [ "KeyH" ], up = [ "KeyT" ], down = [ "KeyG" ] }

        _ ->
            { left = [ "KeyJ", "Numpad4" ], right = [ "KeyL", "Numpad6" ], up = [ "KeyI", "Numpad8" ], down = [ "KeyK", "Numpad5", "Numpad2" ] }


readPads : Model -> List ( Held, Pad )
readPads model =
    List.map
        (\i ->
            let
                m =
                    keyMap i

                any codes set =
                    List.any (\c -> Set.member c set) codes

                raw =
                    Maybe.withDefault { left = False, right = False, up = False, down = False, start = False } (listGet i model.pads)

                held =
                    { left = any m.left model.keys || raw.left
                    , right = any m.right model.keys || raw.right
                    , up = any m.up model.keys || raw.up
                    , down = any m.down model.keys || raw.down
                    }

                prev =
                    Maybe.withDefault noHeld (listGet i model.prevHeld)
            in
            ( held
            , { left = held.left
              , right = held.right
              , up = held.up
              , down = held.down
              , upP = (held.up && not prev.up) || any m.up model.tapped
              , downP = (held.down && not prev.down) || any m.down model.tapped
              , leftP = (held.left && not prev.left) || any m.left model.tapped
              , rightP = (held.right && not prev.right) || any m.right model.tapped
              }
            )
        )
        (List.range 0 3)


rawPadDecoder : D.Decoder (List RawPad)
rawPadDecoder =
    D.list
        (D.map5 RawPad
            (D.field "left" D.bool)
            (D.field "right" D.bool)
            (D.field "up" D.bool)
            (D.field "down" D.bool)
            (D.field "start" D.bool)
        )



-- UPDATE -----------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown code repeat ->
            ( { model
                | keys = Set.insert code model.keys
                , tapped =
                    if repeat then
                        model.tapped

                    else
                        Set.insert code model.tapped
              }
            , Cmd.none
            )

        KeyUp code ->
            ( { model | keys = Set.remove code model.keys }, Cmd.none )

        ClearKeys ->
            ( { model | keys = Set.empty }, Cmd.none )

        Gamepads v ->
            ( { model | pads = Result.withDefault [] (D.decodeValue rawPadDecoder v) }, Cmd.none )

        Tick ms ->
            tick (min 0.033 (ms / 1000)) model


tick : Float -> Model -> ( Model, Cmd Msg )
tick dt model0 =
    let
        inputs =
            readPads model0

        pads =
            List.map Tuple.second inputs

        padAt i =
            Maybe.withDefault { left = False, right = False, up = False, down = False, upP = False, downP = False, leftP = False, rightP = False } (listGet i pads)

        padStart =
            List.any .start model0.pads

        tapped c =
            Set.member c model0.tapped

        startP =
            tapped "Enter" || tapped "Space" || tapped "NumpadEnter" || (padStart && not model0.prevStart)

        pauseP =
            tapped "Escape"

        model =
            { model0
                | time = model0.time + dt
                , screenT = model0.screenT + dt
                , muted =
                    if tapped "KeyM" then
                        not model0.muted

                    else
                        model0.muted
                , prevHeld = List.map Tuple.first inputs
                , tapped = Set.empty
                , prevStart = padStart
            }

        ( next, uiSfx ) =
            stepScreen dt startP pauseP tapped padAt model

        ( draws, worldSfx, wantMusic ) =
            view next

        musicCmd =
            if wantMusic /= next.music then
                Ports.music wantMusic

            else
                Cmd.none
    in
    ( { next | music = wantMusic }
    , Cmd.batch
        [ Ports.frame
            (E.object
                [ ( "draws", Render.encode draws )
                , ( "sfx", E.list E.string (uiSfx ++ worldSfx) )
                , ( "muted", E.bool next.muted )
                ]
            )
        , musicCmd
        ]
    )


goto : Screen -> Model -> Model
goto s m =
    { m | screen = s, screenT = 0 }


stepScreen : Float -> Bool -> Bool -> (String -> Bool) -> (Int -> Pad) -> Model -> ( Model, List String )
stepScreen dt startP pauseP tapped padAt model =
    case model.screen of
        Title ->
            let
                toggle i on =
                    let
                        p =
                            padAt i
                    in
                    if p.upP && not on then
                        ( True, True )

                    else if p.downP && on then
                        ( False, True )

                    else
                        ( on, False )

                results =
                    List.indexedMap toggle model.joined

                joined =
                    List.map Tuple.first results

                changed =
                    List.any Tuple.second results

                -- number keys pick the starting level (0 = 10, minus = summit)
                picked =
                    List.range 1 11
                        |> List.filter (\n -> tapped (levelKey n))
                        |> List.head

                startLevel =
                    Maybe.withDefault model.startLevel picked
            in
            if startP && List.any identity joined then
                ( goto Select { model | joined = joined, startLevel = startLevel, ready = [ False, False, False, False ], readyT = 0 }, [ "select" ] )

            else
                ( { model | joined = joined, startLevel = startLevel }
                , if changed || picked /= Nothing then
                    [ "select" ]

                  else
                    []
                )

        Select ->
            stepSelect dt startP padAt model

        Story ->
            if model.screenT > 0.5 && startP then
                let
                    players =
                        model.joined
                            |> List.indexedMap Tuple.pair
                            |> List.filter Tuple.second
                            |> List.map (\( i, _ ) -> Make.player i (Data.characterAt (Maybe.withDefault 0 (listGet i model.picks))))
                in
                ( goto (Playing (World.new model.startLevel players) False) model, [ "select" ] )

            else
                ( model, [] )

        Playing w paused ->
            if model.debug && tapped "KeyB" && w.phase == Climb then
                ( { model | screen = Playing (World.skipToBoss w) False }, [] )

            else if model.debug && tapped "KeyN" then
                ( { model | screen = Playing { w | boss = Maybe.map (\b -> { b | hp = 0 }) w.boss } False }, [] )

            else if pauseP then
                ( { model | screen = Playing w (not paused) }, [ "select" ] )

            else if paused then
                ( model, [] )

            else
                let
                    w2 =
                        World.step dt padAt w
                in
                case w2.outcome of
                    LevelWon ->
                        if w2.level.n >= Data.levelCount then
                            ( goto (Victory w2.players) model, [ "win" ] )

                        else
                            let
                                taunt =
                                    if w2.level.n + 1 == Data.levelCount then
                                        "OUCH! FINE... MY DRAGON WILL ROAST YOU!"

                                    else
                                        Maybe.withDefault "MINE!" (listGet (modBy (List.length Data.kingTaunts) model.tauntIdx) Data.kingTaunts)
                            in
                            ( goto (Interlude (w2.level.n + 1) taunt w2.players) { model | tauntIdx = model.tauntIdx + 1 + floor (rnd (round (model.time * 1000)) * 3) }
                            , []
                            )

                    GameLost ->
                        ( goto (GameOver w2) model, [] )

                    Ongoing ->
                        ( { model | screen = Playing w2 False }, [] )

        Interlude n _ players ->
            if model.screenT > 1 && startP then
                ( goto (Playing (World.new n players) False) model, [ "select" ] )

            else
                ( model, [] )

        GameOver w ->
            if model.screenT > 1 && startP then
                let
                    -- retry the level: everyone back to full health, weapons kept
                    players =
                        List.map (\p -> { p | hp = 100 }) w.players
                in
                ( goto (Playing (World.new w.level.n players) False) model, [ "select" ] )

            else
                ( model, [] )

        Victory _ ->
            if model.screenT > 2 && startP then
                ( goto Title { model | joined = [ False, False, False, False ] }, [ "select" ] )

            else
                ( model, [] )


view : Model -> ( List Render.Draw, List String, String )
view model =
    case model.screen of
        Title ->
            ( Render.title model.time model.joined model.startLevel model.titleBoss, [], "title" )

        Select ->
            ( Render.characterSelect model.time model.joined model.picks model.ready, [], "title" )

        Story ->
            ( Render.story model.time, [], "title" )

        Playing w paused ->
            ( Render.world paused model.muted w
            , w.sfx
            , case w.phase of
                Climb ->
                    "climb"

                Cleared _ ->
                    "none"

                AllKO _ ->
                    "none"

                _ ->
                    "boss"
            )

        Interlude n taunt players ->
            ( Render.interlude model.time n taunt players, [], "title" )

        GameOver w ->
            ( Render.gameOver model.time w, [], "none" )

        Victory players ->
            ( Render.victory model.time players, [], "win" )



-- SUBSCRIPTIONS ------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrameDelta Tick
        , Events.onKeyDown (D.map2 KeyDown (D.field "code" D.string) (D.oneOf [ D.field "repeat" D.bool, D.succeed False ]))
        , Events.onKeyUp (D.map KeyUp (D.field "code" D.string))
        , Events.onVisibilityChange (\_ -> ClearKeys)
        , Ports.gamepads Gamepads
        ]


levelKey : Int -> String
levelKey n =
    if n == 10 then
        "Digit0"

    else if n == 11 then
        "Minus"

    else
        "Digit" ++ String.fromInt n


{-| One player's slot on the select screen: joined?, character index, locked in?
-}
type alias Slot =
    ( Bool, Int, Bool )


{-| Each joined player picks a character: left/right to browse, down to lock
in, up to change their mind. A locked-in character can't be picked by anyone
else. Once everyone is ready the game starts.
-}
stepSelect : Float -> Bool -> (Int -> Pad) -> Model -> ( Model, List String )
stepSelect dt startP padAt model =
    let
        count =
            List.length Data.characters

        voice i =
            "voice_" ++ (Data.characterInfo (Data.characterAt i)).key

        setAt i s slots =
            List.indexedMap
                (\j x ->
                    if j == i then
                        s

                    else
                        x
                )
                slots

        -- characters locked in by everyone except player i
        lockedExcept i slots =
            slots
                |> List.indexedMap Tuple.pair
                |> List.filterMap
                    (\( j, ( on, pick, isReady ) ) ->
                        if j /= i && on && isReady then
                            Just pick

                        else
                            Nothing
                    )

        -- the first character from `start` (stepping by dir) that nobody has locked
        nextFree dir start taken =
            let
                go k pick =
                    if k >= count || not (List.member pick taken) then
                        pick

                    else
                        go (k + 1) (modBy count (pick + dir))
            in
            go 0 (modBy count start)

        stepOne i ( slots, sounds ) =
            case listGet i slots of
                Just ( True, pick, isReady ) ->
                    let
                        p =
                            padAt i

                        taken =
                            lockedExcept i slots
                    in
                    if isReady then
                        if p.upP then
                            ( setAt i ( True, pick, False ) slots, sounds ++ [ "select" ] )

                        else
                            ( slots, sounds )

                    else if p.downP then
                        if List.member pick taken then
                            ( slots, sounds ++ [ "hurt" ] )

                        else
                            ( setAt i ( True, pick, True ) slots, sounds ++ [ voice pick, "pickup" ] )

                    else if p.leftP then
                        let
                            np =
                                nextFree -1 (pick - 1) taken
                        in
                        ( setAt i ( True, np, False ) slots, sounds ++ [ voice np ] )

                    else if p.rightP then
                        let
                            np =
                                nextFree 1 (pick + 1) taken
                        in
                        ( setAt i ( True, np, False ) slots, sounds ++ [ voice np ] )

                    else
                        ( slots, sounds )

                _ ->
                    ( slots, sounds )

        -- anyone browsing a character that just got locked is bumped to a free one
        bump slots =
            List.foldl
                (\i acc ->
                    case listGet i acc of
                        Just ( True, pick, False ) ->
                            let
                                taken =
                                    lockedExcept i acc
                            in
                            if List.member pick taken then
                                setAt i ( True, nextFree 1 pick taken, False ) acc

                            else
                                acc

                        _ ->
                            acc
                )
                slots
                (List.range 0 3)

        -- Enter starts right away: lock everyone in on a free character
        lockAll slots =
            List.foldl
                (\i acc ->
                    case listGet i acc of
                        Just ( True, pick, False ) ->
                            setAt i ( True, nextFree 1 pick (lockedExcept i acc), True ) acc

                        _ ->
                            acc
                )
                slots
                (List.range 0 3)

        initial =
            List.map3 (\on pick isReady -> ( on, pick, isReady )) model.joined model.picks model.ready

        ( stepped, selectSounds ) =
            List.foldl stepOne ( initial, [] ) (List.range 0 3)

        slots1 =
            bump stepped

        picksOf =
            List.map (\( _, pick, _ ) -> pick)

        readyOf =
            List.map (\( _, _, isReady ) -> isReady)

        everyoneReady =
            List.all (\( on, _, isReady ) -> not on || isReady) slots1

        readyT =
            if everyoneReady then
                model.readyT + dt

            else
                0
    in
    if (model.screenT > 0.3 && startP) || readyT > 0.8 then
        let
            final =
                lockAll slots1
        in
        ( goto Story { model | picks = picksOf final, ready = readyOf final, readyT = 0 }, selectSounds ++ [ "select" ] )

    else
        ( { model | picks = picksOf slots1, ready = readyOf slots1, readyT = readyT }, selectSounds )
