# said

Erlang websocket based chat program.

## Build

```Bash
rebar3 compile
rebar3 release
```

## Run

```Bash
./_build/default/rel/said/bin/said foreground
xdg-open http://localhost:8080
```

## Podman

```Bash
podman build --tag ajb0wers/said .
podman run -p 8080:8080 --rm ajb0wers/said
```
