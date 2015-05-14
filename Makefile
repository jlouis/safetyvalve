PROJECT = safetyvalve
REBAR = rebar3

ERLC_OPTS = +debug_info '+{parse_transform, lager_transform}'

DEPS = lager
dep_lager = http://github.com/basho/lager.git 2.0.3

include erlang.mk

eqc-ci: all
	erlc -o ebin eqc_test/svq.erl

.PHONY: rebar3-compile
rebar3-compile:
	$(REBAR) compile | sed -e 's|_build/default/lib/safetyvalve/||'