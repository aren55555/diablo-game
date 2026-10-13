# Stage 1: Build (runs on the host's native platform to avoid emulation issues)
FROM --platform=$BUILDPLATFORM oven/bun:1 AS builder
# Keep in sync with "npm:@lydell/elm" in mise.toml
ARG ELM_VERSION=0.19.1-14
RUN bun install -g @lydell/elm@${ELM_VERSION}
WORKDIR /app
COPY package.json bun.lock ./
RUN bun install --frozen-lockfile
COPY . .
RUN bun scripts/build.ts --optimize

# Stage 2: Serve
FROM joseluisq/static-web-server:2
COPY --from=builder /app/dist/index.html /app/dist/elm.js /app/dist/main.js /public/
ENV SERVER_ROOT=/public
