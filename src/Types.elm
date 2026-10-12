module Types exposing (..)

{-| All game-state types.
-}


type Weapon
    = Fists
    | Nunchucks
    | Sword
    | MachineGun
    | Bazooka
    | FartBomb
    | Laser


{-| One player's d-pad for this frame. up = jump, down = attack/grab.
-}
type alias Pad =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , upP : Bool
    , downP : Bool
    , leftP : Bool
    , rightP : Bool
    }


{-| The playable characters. Each has its own sprite, voice and aura.
-}
type Character
    = Goblin
    | Fairy
    | PopStar
    | Dino
    | TreeMan
    | PieWoman
    | BlackBerry
    | BlockMan
    | PoopyDiaper
    | Ghosty
    | BeerMan
    | Spider


type alias Player =
    { idx : Int
    , character : Character
    , x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , facing : Float
    , onGround : Bool
    , standing : Int
    , coyote : Float
    , jumpBuf : Float
    , airJumps : Int
    , cd : Float
    , atkT : Float
    , invuln : Float
    , slow : Float
    , poison : Float
    , hurtFx : Float
    , hp : Float
    , weapon : Weapon
    , gold : Int
    , diamonds : Int
    , kills : Int
    , anim : Float
    , ghostT : Float
    , prompt : Float
    , auraT : Float
    , voiceT : Float
    }


type PlatKind
    = Ground
    | Normal
    | Arena


type alias Mover =
    { x0 : Float, range : Float, speed : Float, phase : Float }


type alias Platform =
    { id : Int
    , x : Float
    , y : Float
    , w : Float
    , h : Float
    , kind : PlatKind
    , moving : Maybe Mover
    , crumble : Bool
    , ct : Float
    , gone : Bool
    , respawnT : Float
    , dx : Float
    , ice : Bool
    }


type EnemyKind
    = Goon
    | BabyDragon
    | Archer


type alias Enemy =
    { id : Int
    , kind : EnemyKind
    , x : Float
    , y : Float
    , w : Float
    , h : Float
    , vx : Float
    , vy : Float
    , facing : Float
    , t : Float
    , flash : Float
    , hp : Float
    , maxHp : Float
    , plat : Int
    , homeX : Float
    , homeY : Float
    , shootT : Float
    , lvl : Int
    , dead : Bool
    , active : Bool
    }


type ProjKind
    = Bullet
    | Rocket
    | FartP
    | Magic
    | Fireball
    | Spear
    | Rock
    | Sonic
    | MoneyBag
    | CoinP
    | Snowball
    | Icicle
    | Arrow
    | Bone
    | DiamondP
    | BitcoinP
    | Wave


type Owner
    = ByPlayer Int
    | Hostile


type alias Proj =
    { id : Int
    , kind : ProjKind
    , x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , r : Float
    , dmg : Float
    , life : Float
    , grav : Float
    , owner : Owner
    , pierce : Bool
    , hits : List Int
    , slow : Float
    , floorBreak : Bool
    , shadow : Bool
    , explode : Float
    }


type alias Zone =
    { x : Float
    , y : Float
    , r : Float
    , life : Float
    , maxLife : Float
    , dps : Float
    , hostile : Bool
    , t : Float
    }


type alias Beam =
    { x1 : Float, y1 : Float, x2 : Float, y2 : Float, life : Float, color : String }


type PKind
    = Dot
    | Puff
    | Ring
    | Txt String
    | Img String Bool


type alias Particle =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , life : Float
    , maxLife : Float
    , size : Float
    , grav : Float
    , color : String
    , kind : PKind
    }


type ItemKind
    = Coin
    | Diamond
    | Heart
    | WeaponItem Weapon


type alias Item =
    { id : Int
    , kind : ItemKind
    , x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , phys : Bool
    , t : Float
    , life : Float
    , gone : Bool
    }


type BreathKind
    = Fire
    | Ice
    | Poison


type AttackSpec
    = Charge
    | Swoop
    | Shoot ProjKind Int Float Float Bool
    | Radial ProjKind Int Float
    | Stomp
    | Rain ProjKind Int
    | Summon EnemyKind Int
    | LaserA Int
    | BreathA BreathKind


type alias BossDef =
    { name : String
    , sub : String
    , hp : Float
    , w : Float
    , h : Float
    , fly : Bool
    , hover : Float
    , speed : Float
    , sprite : String
    , mouth : ( Float, Float )
    , eyes : List ( Float, Float )
    , attacks : List AttackSpec
    , quotes : List String
    }


type alias Attack =
    { spec : AttackSpec
    , specIdx : Int
    , tt : Float
    , stage : Int
    , fired : Int
    , tx : Float
    , ty : Float
    }


type BossState
    = Sleeping
    | Fighting
    | Defeated


type alias BossBeam =
    { eye : ( Float, Float ), sx : Float, ex : Float, px : Float, on : Bool }


type alias Breath =
    { kind : BreathKind, ang : Float, t : Float, dropT : Float }


type alias Boss =
    { def : BossDef
    , n : Int
    , x : Float
    , y : Float
    , w : Float
    , h : Float
    , vx : Float
    , vy : Float
    , facing : Float
    , t : Float
    , flash : Float
    , hp : Float
    , maxHp : Float
    , state : BossState
    , atk : Maybe Attack
    , idleT : Float
    , lastAtk : Int
    , enraged : Bool
    , tele : Float
    , quote : String
    , quoteT : Float
    , quoteIdx : Int
    , onGround : Bool
    , landed : Bool
    , beams : List BossBeam
    , breath : Maybe Breath
    , floorY : Float
    , dmgMul : Float
    }


type WeatherFx
    = NoFx
    | Snow
    | Ember
    | Dust
    | Sparkle


type Deco
    = Grass
    | Stone
    | Crystal
    | SnowCap
    | Lava
    | GoldTrim


type alias Theme =
    { sky : List String
    , far : String
    , near : String
    , top : String
    , body : String
    , edge : String
    , fx : WeatherFx
    , deco : Deco
    , ice : Bool
    , stars : Bool
    }


type alias Level =
    { n : Int
    , name : String
    , theme : Theme
    , platforms : List Platform
    , height : Float
    , arenaY : Float
    , arenaCamTop : Float
    , hazards : Bool
    }


type Phase
    = Climb
    | Gather Float
    | BossIntro Float
    | BossFight
    | Cleared Float
    | AllKO Float


type alias Banner =
    { text : String, sub : String, t : Float, color : String }


type Outcome
    = Ongoing
    | LevelWon
    | GameLost


type alias Warning =
    { x : Float, t : Float }


type alias World =
    { level : Level
    , players : List Player
    , enemies : List Enemy
    , projs : List Proj
    , zones : List Zone
    , beams : List Beam
    , particles : List Particle
    , items : List Item
    , boss : Maybe Boss
    , camY : Float
    , shake : Float
    , phase : Phase
    , banner : Maybe Banner
    , time : Float
    , tick : Int
    , nextId : Int
    , sfx : List String
    , hazardT : Float
    , heartT : Float
    , warnings : List Warning
    , outcome : Outcome
    }
