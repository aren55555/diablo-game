# Container runtime: docker if on PATH, else podman. Override with `just container_cmd=... container <recipe>`
container_cmd := `command -v docker >/dev/null && echo docker || { command -v podman >/dev/null && echo podman || true; }`

image := "docker.io/aren55555/mount-diablo:latest"

[private]
default:
    @just --justfile "{{source_file()}}" --list

# Build the game into a static-web-server container image
build:
    @test -n "{{ container_cmd }}" || { echo "error: no container runtime found (tried docker and podman)" >&2; exit 1; }
    {{ container_cmd }} build \
        --platform linux/amd64 \
        -f "{{justfile_directory()}}/Containerfile" \
        -t {{ image }} \
        "{{justfile_directory()}}"

# Build and push the container image to Docker Hub
push: build
    {{ container_cmd }} push {{ image }}

# Run the container image on http://localhost:8080
run port="8080": build
    {{ container_cmd }} run --rm -p {{ port }}:80 {{ image }}
