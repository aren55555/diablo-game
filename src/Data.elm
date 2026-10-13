module Data exposing
    ( CharacterInfo
    , PlayerDef
    , WeaponDef
    , bossDefs
    , bossFor
    , characterAt
    , characterInfo
    , characters
    , kingTaunts
    , levelCount
    , levelInfo
    , playerDef
    , spriteFor
    , weaponDef
    )

{-| Static game content: players, weapons, themes, levels and bosses.
All distances are in low-res game pixels (384x240 screen).
-}

import Types exposing (..)
import Util exposing (listGet)


type alias PlayerDef =
    { name : String, color : String, dark : String, keys : String, jump : String, atk : String }


playerDef : Int -> PlayerDef
playerDef i =
    case i of
        0 ->
            { name = "P1", color = "#e83c3c", dark = "#8c1c1c", keys = "W A S D", jump = "W", atk = "S" }

        1 ->
            { name = "P2", color = "#3c8cf8", dark = "#1c3c9c", keys = "ARROWS", jump = "UP", atk = "DOWN" }

        2 ->
            { name = "P3", color = "#3cd860", dark = "#1c7c30", keys = "T F G H", jump = "T", atk = "G" }

        _ ->
            { name = "P4", color = "#f8c818", dark = "#a06c00", keys = "I J K L", jump = "I", atk = "K" }


type alias WeaponDef =
    { name : String, cd : Float, anim : Float, sprite : String }


weaponDef : Weapon -> WeaponDef
weaponDef wp =
    case wp of
        Fists ->
            WeaponDef "FISTS" 0.32 0.15 "w_fists"

        Nunchucks ->
            WeaponDef "NUNCHUCKS" 0.17 0.17 "w_nunchucks"

        Sword ->
            WeaponDef "ENCHANTED SWORD" 0.38 0.25 "w_sword"

        MachineGun ->
            WeaponDef "MACHINE GUN" 0.07 0.07 "w_machinegun"

        Bazooka ->
            WeaponDef "BAZOOKA" 0.95 0.3 "w_bazooka"

        FartBomb ->
            WeaponDef "STINKY FART BOMB" 1.1 0.3 "w_fartbomb"

        Laser ->
            WeaponDef "LASER BLASTER" 0.28 0.12 "w_laser"



-- THEMES ---------------------------------------------------------------------


meadow : Theme
meadow =
    { sky = [ "#3cbcfc", "#5cc8fc", "#7cd4fc", "#a4e4fc", "#c4f0fc" ], far = "#78a8b8", near = "#38884c", top = "#58d854", body = "#a0582c", edge = "#503000", fx = NoFx, deco = Grass, ice = False, stars = False }


sand : Theme
sand =
    { sky = [ "#e87818", "#f89c38", "#fcbc58", "#fcd890", "#fcecc0" ], far = "#e0a868", near = "#c8843c", top = "#f8d878", body = "#d0a050", edge = "#805020", fx = Sandstorm, deco = Cactus, ice = False, stars = False }


miami : Theme
miami =
    { sky = [ "#3cbcfc", "#7cd4fc", "#f8a4c8", "#fcc4a0", "#fce4a0" ], far = "#f8b8d8", near = "#1c6c64", top = "#fce4a0", body = "#c08850", edge = "#6c4420", fx = Seagulls, deco = Palms, ice = False, stars = False }


rocky : Theme
rocky =
    { sky = [ "#e45c10", "#f87838", "#fc9c58", "#fcbc80", "#fce0a8" ], far = "#b88c70", near = "#7c5c4c", top = "#bcbcbc", body = "#7c6c64", edge = "#3c302c", fx = NoFx, deco = Stone, ice = False, stars = False }


cave : Theme
cave =
    { sky = [ "#1c0c3c", "#2c1c5c", "#442c7c", "#5c3c94", "#7454ac" ], far = "#3c2c64", near = "#241840", top = "#9c7cfc", body = "#4c3c74", edge = "#1c1430", fx = Dust, deco = Crystal, ice = False, stars = False }


snow : Theme
snow =
    { sky = [ "#6c88b8", "#8ca4cc", "#acc0dc", "#ccdcec", "#ecf4fc" ], far = "#c4d4e8", near = "#8ca0bc", top = "#fcfcfc", body = "#8ca4bc", edge = "#4c6078", fx = Snow, deco = SnowCap, ice = True, stars = False }


circuit : Theme
circuit =
    { sky = [ "#020818", "#041030", "#082048", "#0c3060", "#104070" ], far = "#0c2848", near = "#081a30", top = "#5c6878", body = "#20242c", edge = "#101418", fx = Electric, deco = Circuit, ice = False, stars = False }


bone : Theme
bone =
    { sky = [ "#3c4454", "#545c70", "#6c788c", "#9ca4b4", "#c4ccd8" ], far = "#848c9c", near = "#545c6c", top = "#ececec", body = "#747c88", edge = "#3c4048", fx = Snow, deco = SnowCap, ice = True, stars = False }


lava : Theme
lava =
    { sky = [ "#200404", "#400c08", "#68140c", "#902410", "#b83c14" ], far = "#58140c", near = "#2c0c08", top = "#fc7838", body = "#3c2c28", edge = "#180c08", fx = Ember, deco = Lava, ice = False, stars = False }


dusk : Theme
dusk =
    { sky = [ "#2c2c6c", "#503c7c", "#7c4c84", "#b4647c", "#e48c6c" ], far = "#6c5884", near = "#443858", top = "#a4a4a4", body = "#5c5868", edge = "#343040", fx = NoFx, deco = Stone, ice = False, stars = True }


gold : Theme
gold =
    { sky = [ "#3c104c", "#5c1c5c", "#8c3c5c", "#c4644c", "#f8a444" ], far = "#784468", near = "#4c2844", top = "#fcd000", body = "#a87c20", edge = "#604000", fx = Sparkle, deco = GoldTrim, ice = False, stars = True }


summit : Theme
summit =
    { sky = [ "#08040c", "#140818", "#240c24", "#3c0c2c", "#5c1030" ], far = "#2c1430", near = "#180a1c", top = "#6c4c3c", body = "#2c201c", edge = "#100808", fx = Ember, deco = Lava, ice = False, stars = True }



-- LEVELS ---------------------------------------------------------------------


levelCount : Int
levelCount =
    11


levelInfo : Int -> { name : String, theme : Theme, weapons : List Weapon }
levelInfo n =
    case n of
        1 ->
            { name = "FOOTHILL MEADOW", theme = meadow, weapons = [ Nunchucks, FartBomb, Sword ] }

        2 ->
            { name = "SOUTH BEACH", theme = miami, weapons = [ Nunchucks, FartBomb, Sword ] }

        3 ->
            { name = "SANDSTONE LEDGES", theme = sand, weapons = [ Sword, FartBomb, MachineGun, Nunchucks ] }

        4 ->
            { name = "BAT CAVES", theme = cave, weapons = [ MachineGun, Sword, FartBomb, Nunchucks ] }

        5 ->
            { name = "TROLL TOLL BRIDGE", theme = rocky, weapons = [ Bazooka, MachineGun, FartBomb, Sword ] }

        6 ->
            { name = "FROSTY CLIFFS", theme = snow, weapons = [ Bazooka, Sword, MachineGun, FartBomb ] }

        7 ->
            { name = "CIRCUIT CITADEL", theme = circuit, weapons = [ Laser, Bazooka, FartBomb, MachineGun ] }

        8 ->
            { name = "LAVA STEPS", theme = lava, weapons = [ Laser, MachineGun, Bazooka, Sword ] }

        9 ->
            { name = "CYCLOPS CRAGS", theme = dusk, weapons = [ Laser, Bazooka, FartBomb, MachineGun, Sword ] }

        10 ->
            { name = "THE KING'S GOLDEN STAIR", theme = gold, weapons = [ Laser, Bazooka, FartBomb, MachineGun, Sword, Nunchucks ] }

        _ ->
            { name = "SUMMIT OF MOUNT DIABLO", theme = summit, weapons = [ Laser, Bazooka, Sword, FartBomb, MachineGun ] }



-- BOSSES ---------------------------------------------------------------------


boss : String -> String -> Float -> ( Float, Float ) -> Float -> String -> List AttackSpec -> List String -> BossDef
boss name sub hp ( w, h ) speed sprite attacks quotes =
    { name = name
    , sub = sub
    , hp = hp
    , w = w
    , h = h
    , fly = False
    , hover = 0
    , speed = speed
    , sprite = sprite
    , mouth = ( w * 0.4, -h * 0.7 )
    , eyes = []
    , attacks = attacks
    , quotes = quotes
    , coward = False
    }


bossDefs : List BossDef
bossDefs =
    [ boss "GOON CAPTAIN"
        "THE KING'S BIGGEST BULLY"
        520
        ( 28, 38 )
        34
        "b_captain"
        [ Charge, Shoot Spear 3 0.25 168 False, Summon Goon 2 ]
        [ "FOR THE KING!", "NO CLIMBING ALLOWED!" ]
    , let
        b =
            boss "SHADY BILLIONAIRE"
                "THE KING'S RICH BEST FRIEND"
                450
                ( 30, 44 )
                64
                "b_billionaire"
                [ Summon Bodyguard 2, ThrowCash 6, Jet 6, Summon Bodyguard 2 ]
                [ "GET HIM, BOYS!", "I KNOW EVERYONE!", "MY LAWYERS WILL HANDLE THIS!", "NO PICTURES!", "I'LL JUST BUY THIS MOUNTAIN!", "HAVE YOUR PEOPLE CALL MY PEOPLE!" ]
      in
      -- never attacks himself: he runs away and lets his bodyguards fight
      { b | coward = True }
    , boss "BOULDER GOLEM"
        "ROCKS FOR BRAINS"
        850
        ( 40, 46 )
        22
        "b_golem"
        [ Stomp, Rain Rock 8, Shoot Rock 3 0.35 168 True ]
        [ "ROCK SMASH!", "GRRRRIND..." ]
    , let
        b =
            boss "COUNT FLAPULA"
                "BAT BARON OF THE CAVES"
                950
                ( 50, 26 )
                68
                "b_bat"
                [ Swoop, Radial Sonic 10 104, Summon BabyDragon 2 ]
                [ "I VANT YOUR GOLD!", "BLEH BLEH!" ]
      in
      { b | fly = True, hover = 72 }
    , boss "TAX TROLL"
        "COLLECTS THE KING'S TAXES"
        1100
        ( 40, 44 )
        36
        "b_troll"
        [ Charge, Shoot MoneyBag 4 0.5 176 True, Rain CoinP 12, Stomp ]
        [ "PAY THE TOLL!", "TAXES FOR THE KING!" ]
    , boss "BIG FROSTY"
        "THE GRUMPY YETI"
        1250
        ( 42, 46 )
        38
        "b_yeti"
        [ Stomp, Shoot Snowball 5 0.5 168 True, Charge, Rain Icicle 10 ]
        [ "BRRRRAAAH!", "CHILL OUT!" ]
    , let
        b =
            boss "CRYPTO CHAD"
                "TECH BRO AND SERIAL FOUNDER"
                1400
                ( 30, 44 )
                48
                "b_crypto"
                [ Charge, LaserA 2, Rain BitcoinP 12, Shoot Rocket 3 0.4 180 False, Radial BitcoinP 10 120 ]
                [ "I'M RAISING VC!", "BTC TO THE MOON!", "HAVE FUN STAYING POOR!", "WAGMI!", "IT'S NOT A BUBBLE!", "DIAMOND HANDS!", "MY AI WROTE THIS TAUNT!" ]
      in
      -- laser eyes come out of both lenses of his shades
      { b | eyes = [ ( 6, -35 ), ( 1, -35 ) ] }
    , let
        b =
            boss "MAGMA SALAMANDER"
                "HOT HOT HOT!"
                1600
                ( 58, 26 )
                44
                "b_sala"
                [ BreathA Fire, Charge, Rain Fireball 10, Stomp ]
                [ "HSSSSSS!", "FEEL THE HEAT!" ]
      in
      { b | mouth = ( 31, -14 ) }
    , let
        b =
            boss "CYCLOPS GRUMBLE"
                "ONE EYE, BIG TEMPER"
                1800
                ( 44, 56 )
                30
                "b_cyclops"
                [ LaserA 1, Stomp, Rain Rock 12, Charge ]
                [ "I SEE YOU!", "GRUMBLE GRUMBLE!" ]
      in
      { b | eyes = [ ( 0, -48 ) ] }
    , boss "THE EVIL KING"
        "GREEDY, ORANGE AND MEAN"
        2200
        ( 52, 54 )
        28
        "b_king"
        [ Shoot CoinP 7 0.8 172 False, Stomp, Summon Goon 3, Rain DiamondP 14, Rain BitcoinP 12, Radial DiamondP 12 128 ]
        [ "MINE! ALL MINE!", "I DON'T SHARE!", "GET YOUR OWN GOLD!", "MY DIAMONDS!!", "SHARING IS FOR LOSERS!", "YOUR BITCOIN IS MY BITCOIN!", "HODL? I'LL HOLD IT FOR YOU!" ]
    , let
        b =
            boss "DIABLO"
                "THE DRAGON OF MOUNT DIABLO"
                5000
                ( 84, 60 )
                60
                "b_dragon"
                [ BreathA Fire, BreathA Ice, BreathA Poison, LaserA 2, Summon BabyDragon 3, Swoop, Rain Fireball 14 ]
                [ "ROOOOAAAR!", "YOU DARE CLIMB MY MOUNTAIN?", "I WILL ROAST YOU!" ]
      in
      { b | fly = True, hover = 76, mouth = ( 47, -48 ), eyes = [ ( 36, -55 ), ( 30, -55 ) ] }
    ]


bossFor : Int -> BossDef
bossFor n =
    Maybe.withDefault placeholder (listGet (n - 1) bossDefs)


placeholder : BossDef
placeholder =
    boss "?" "?" 100 ( 20, 20 ) 10 "b_captain" [ Charge ] []


kingTaunts : List String
kingTaunts =
    [ "HA! YOU'LL NEVER REACH MY GOLD. IT'S MINE!"
    , "SHARING? NEVER HEARD OF IT!"
    , "EVERY COIN, EVERY DIAMOND... MINE MINE MINE!"
    , "I'M THE RICHEST, ORANGEST KING EVER!"
    , "I TOOK EVERYONE'S CRYPTO. EVERY LAST BITCOIN!"
    , "MY GUARDS WILL STOP YOU! ...PROBABLY!"
    , "I'LL BUY A BIGGER CROWN WITH YOUR GOLD!"
    , "NOBODY GETS A SINGLE COIN. NOT EVEN MY MOM!"
    , "STOP CLIMBING MY MOUNTAIN! I PAID FOR IT!"
    ]



-- CHARACTERS -------------------------------------------------------------------


type alias CharacterInfo =
    { name : String
    , key : String
    , blurb : String
    }


{-| In the order they appear on the character select screen.
-}
characters : List Character
characters =
    [ Goblin, Fairy, PopStar, Dino, TreeMan, PieWoman, BlackBerry, BlockMan, PoopyDiaper, Ghosty, BeerMan, Spider ]


characterAt : Int -> Character
characterAt i =
    Maybe.withDefault Goblin (listGet (modBy (List.length characters) i) characters)


{-| key names the sprites ("c\_<key>_<pose>") and the voice sound ("voice_<key>").
-}
characterInfo : Character -> CharacterInfo
characterInfo c =
    case c of
        Goblin ->
            CharacterInfo "GOBLIN" "goblin" "SNEAKY CACKLE"

        Fairy ->
            CharacterInfo "FAIRY" "fairy" "MAGIC SPARKLE"

        PopStar ->
            CharacterInfo "POP STAR" "popstar" "SINGS A TUNE"

        Dino ->
            CharacterInfo "DINO" "dino" "BIG ROARS"

        TreeMan ->
            CharacterInfo "TREE MAN" "tree" "CREAK CREAK"

        PieWoman ->
            CharacterInfo "PIE WOMAN" "pie" "SPLAT!"

        BlackBerry ->
            CharacterInfo "BLACKBERRY" "dog" "WOOF WOOF!"

        BlockMan ->
            CharacterInfo "BLOCK MAN" "block" "BONK BONK!"

        PoopyDiaper ->
            CharacterInfo "POOPY DIAPER" "poopy" "PFFFFT!"

        Ghosty ->
            CharacterInfo "GHOST" "ghost" "OOOOOOH!"

        BeerMan ->
            CharacterInfo "BEER MAN" "beer" "BUUURP!"

        Spider ->
            CharacterInfo "SPIDER" "spider" "WEB SLINGER"


{-| The sprite to draw for a player this frame.
-}
spriteFor : Player -> String
spriteFor p =
    let
        pose =
            if not p.onGround then
                "jump"

            else if abs p.vx > 10 then
                if modBy 2 (floor (p.anim * 8)) == 0 then
                    "walk1"

                else
                    "walk2"

            else
                "idle"
    in
    "c_" ++ (characterInfo p.character).key ++ "_" ++ pose
