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
    { debug : Bool }


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
            ( Render.title model.time model.joined model.startLevel, [], "title" )

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


{-| Each joined player picks a character: left/right to browse, down to lock
in, up to change their mind. Once everyone is ready the game starts.
-}
stepSelect : Float -> Bool -> (Int -> Pad) -> Model -> ( Model, List String )
stepSelect dt startP padAt model =
    let
        count =
            List.length Data.characters

        voice i =
            "voice_" ++ (Data.characterInfo (Data.characterAt i)).key

        stepOne i ( ( on, pick ), isReady ) =
            let
                p =
                    padAt i
            in
            if not on then
                ( ( pick, False ), [] )

            else if isReady then
                if p.upP then
                    ( ( pick, False ), [ "select" ] )

                else
                    ( ( pick, True ), [] )

            else if p.downP then
                ( ( pick, True ), [ voice pick, "pickup" ] )

            else if p.leftP then
                ( ( modBy count (pick - 1), False ), [ voice (pick - 1) ] )

            else if p.rightP then
                ( ( modBy count (pick + 1), False ), [ voice (pick + 1) ] )

            else
                ( ( pick, False ), [] )

        results =
            List.indexedMap stepOne
                (List.map2 Tuple.pair (List.map2 Tuple.pair model.joined model.picks) model.ready)

        picks =
            List.map (Tuple.first >> Tuple.first) results

        ready =
            List.map (Tuple.first >> Tuple.second) results

        sounds =
            List.concatMap Tuple.second results

        everyoneReady =
            List.all identity (List.map2 (\on r -> not on || r) model.joined ready)

        readyT =
            if everyoneReady then
                model.readyT + dt

            else
                0
    in
    if (model.screenT > 0.3 && startP) || readyT > 0.8 then
        ( goto Story { model | picks = picks, ready = List.map2 (||) ready model.joined, readyT = 0 }, sounds ++ [ "select" ] )

    else
        ( { model | picks = picks, ready = ready, readyT = readyT }, sounds )
