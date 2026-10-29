all: release

release:
	rebar3 compile
	rebar3 release

run:
	./_build/default/rel/said/bin/said foreground

podman:
	podman build --tag ajb0wers/said .
	podman run -p 10000:10000 --rm --name said ajb0wers/said

clean:
	rebar3 clean
	rm -rf ./_build

.PHONY: all clean release run podman
