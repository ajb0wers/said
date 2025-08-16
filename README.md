# Said

Web chat program in Erlang.

## Build

```Bash
rebar3 compile
rebar3 release
```

## Run

```Bash
# Start as a program in foreground
./_build/default/rel/said/bin/said foreground

# e.g. Build & run container using podman or docker
podman build --tag ajb0wers/said .
podman run -p 8080:8080 --rm --name said ajb0wers/said

# Open web page to chat
xdg-open http://localhost:8080
```

