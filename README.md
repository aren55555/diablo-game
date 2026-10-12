# Mount Diablo

A 4-player local co-op climbing game with a retro 8-bit look. Climb 10 levels
of Mount Diablo together, beat a mini boss on each level's top platform, then
face **Diablo**, the dragon at the summit, and take back the gold the greedy,
orange Evil King stole.

## Controls

Every player uses one d-pad: **up = jump** (press again in the air to double
jump) and **down = attack**. Down also swaps for a weapon you're standing on.

| Player | Keys                   |
| ------ | ---------------------- |
| P1     | `W` `A` `S` `D`        |
| P2     | Arrow keys             |
| P3     | `T` `F` `G` `H`        |
| P4     | `I` `J` `K` `L` (or numpad `8` `4` `5` `6`) |

Gamepads also work: pad 1 controls P1, pad 2 controls P2, and so on. Use the
d-pad or stick to move, A to jump, and B or X to attack.

- **Title screen:** press your jump key to join (your down key leaves).
  Press `Enter` to start. Number keys pick a starting level (`0` = level 10,
  `-` = the summit).
- **In game:** `P` / `Esc` pauses, `M` mutes.

Many keyboards can't register lots of keys held at once, so with four players
on one keyboard some combinations may drop. Gamepads avoid that.

## How it plays

- Each player has 0-100 HP. While climbing you slowly heal, and hearts restore 25.
- The top of the screen is a ceiling, so the group has to climb together.
  Fall off the bottom and you're put back near your team (-10 HP).
- Weapons are found along the climb, and you can carry one at a time:
  nunchucks, enchanted sword, machine gun, bazooka, super stinky fart bomb
  and laser blaster. Ranged weapons auto-aim a little.
- If you're knocked out, your ghost comes back after 10 seconds as long as a
  teammate is still standing. If everyone is down, you retry the level.
- The levels get harder as you go: narrower and moving or crumbling
  platforms, ice, falling rocks, archers, and tougher bosses. The bosses are
  the Goon Captain, Broodmother, Boulder Golem, Count Flapula, Tax Troll, Big
  Frosty, Sir Rattlebones, Magma Salamander, Cyclops Grumble, **the Evil
  King**, and finally **Diablo**. Diablo breathes fire, ice and poison and
  shoots red laser eyes.

## Development

The toolchain is managed by [mise](https://mise.jdx.dev) (`mise.toml`). All
commands go through [just](https://just.systems):

```sh
just setup   # install elm, elm-format, bun + TS deps
just dev     # http://localhost:8000 - refresh the page to rebuild
just build   # optimized build in dist/
just check   # type-check Elm + TypeScript
just sim     # headless bots play every level (see justfile for options)
just fmt     # elm-format
```

Dev URL flags: `?debug` turns on `B` (skip to the boss) and `N` (defeat the
boss). `?bgtick` keeps the game running in a background tab, for automated
testing.

### Layout

- `src/` - **Elm**: all game state and logic.
  - `World.elm`: the simulation.
  - `Boss.elm`: boss AI.
  - `Level.elm`: procedural levels.
  - `Sprites.elm` + `Pixel.elm`: pixel art drawn in code.
  - `Render.elm`: scene to draw commands.
- `platform/` - **TypeScript** (strict): draws Elm's per-frame draw commands
  on a 384x240 canvas, renders the 5x7 bitmap font, synthesizes chiptune
  sound effects and music, and polls gamepads. It talks to Elm through the
  ports in `src/Ports.elm`.
- `sim/Sim.elm` + `scripts/sim.ts` - headless smoke test with bot players.
- `scripts/` - bun build and dev server.

Elm comes from `npm:@lydell/elm`, a native arm64/x64 build of the official
0.19.1 compiler. The upstream macOS binary is x86-only.
