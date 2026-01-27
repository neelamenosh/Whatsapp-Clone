.PHONY: all compile release clean deps run shell test

REBAR = rebar3

all: deps compile

deps:
	$(REBAR) deps

compile:
	$(REBAR) compile

release:
	$(REBAR) release

clean:
	$(REBAR) clean
	rm -rf _build

run: release
	_build/default/rel/video_backend/bin/video_backend foreground

shell:
	$(REBAR) shell

test:
	$(REBAR) eunit
	$(REBAR) ct

dialyzer:
	$(REBAR) dialyzer

# Development helpers
dev: compile
	$(REBAR) shell --apps video_signaling,video_web

# Production release
prod:
	$(REBAR) as prod release

# Docker build
docker:
	docker build -t video_backend:latest .

# FreeBSD deployment
freebsd-deps:
	pkg install -y erlang rebar3

# Create Mnesia schema
init-mnesia:
	mkdir -p /var/lib/video_backend/mnesia
	mkdir -p /var/log/video_backend
