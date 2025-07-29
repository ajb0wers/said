# Said

Erlang websocket based chat program.

## Build

```Bash
rebar3 compile
rebar3 release
```

## Run

```Bash
# Start as a program in foreground
./_build/default/rel/said/bin/said foreground

# Open the web page
xdg-open http://localhost:8080
```

## Podman

```Bash
# Dockerfile using podman or docker
podman build --tag ajb0wers/said .
podman run -p 8080:8080 --rm ajb0wers/said
```

