# Mount Diablo

A 4-player local co-op climbing game, written in Elm with a TypeScript
platform layer.

**Play it: https://diablo.arenpatel.com/** (deployed from `main`
by `.github/workflows/pages.yml`)

## Prerequisites

Install [mise](https://mise.jdx.dev) and [just](https://just.systems). mise
manages the rest of the toolchain (Elm, elm-format, bun), as pinned in
`mise.toml`.

Elm comes from `npm:@lydell/elm`, a native arm64/x64 build of the official
0.19.1 compiler. The upstream macOS binary is x86-only.

## Install

```sh
just setup   # install elm, elm-format, bun + TS deps
```

## Run

```sh
just dev     # http://localhost:8000 - refresh the page to rebuild
just build   # optimized build in dist/ (open dist/index.html or host it anywhere)
```

`just dev 3000` serves on a different port.

## Develop

```sh
just check      # type-check Elm + TypeScript
just sim        # headless bots play every level (see justfile for options)
just fmt        # elm-format
just fmt-check  # verify formatting without changing files
just clean      # remove dist/, elm-stuff/, node_modules/
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
  - `Aura.elm`: character auras and voices.
  - `Render.elm`: scene to draw commands.
- `platform/` - **TypeScript** (strict): draws Elm's per-frame draw commands
  on a 384x240 canvas, renders the 5x7 bitmap font, synthesizes chiptune
  sound effects and music, and polls gamepads. It talks to Elm through the
  ports in `src/Ports.elm`.
- `sim/Sim.elm` + `scripts/sim.ts` - headless smoke test with bot players.
- `scripts/` - bun build and dev server.
