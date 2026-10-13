# Mount Diablo - every command runs inside the mise toolchain (see mise.toml).

set shell := ["bash", "-euo", "pipefail", "-c"]

mise := "mise exec --"

mod container

# List available recipes
default:
    @just --list --list-submodules

# Install the toolchain (elm, elm-format, bun) and TypeScript deps
setup:
    mise install
    {{mise}} bun install

# Dev server on http://localhost:8000 - refresh the page to rebuild
dev port="8000": setup
    {{mise}} bun scripts/serve.ts --port {{port}}

# Optimized production build into dist/ (open dist/index.html or host it anywhere)
build: setup
    {{mise}} bun scripts/build.ts --optimize

# Type-check Elm and TypeScript
check: setup
    {{mise}} elm make src/Main.elm --output=/dev/null
    {{mise}} bunx tsc --noEmit

# Headless smoke test: bots climb every level and fight every boss
# e.g. `just sim 1-11 240 4 laser --boss`
sim *args: setup
    {{mise}} bun scripts/sim.ts {{args}}

# Format Elm sources
fmt:
    {{mise}} elm-format src sim --yes

# Verify formatting without changing files
fmt-check:
    {{mise}} elm-format src sim --validate

# Remove build output and caches
clean:
    rm -rf dist elm-stuff node_modules
