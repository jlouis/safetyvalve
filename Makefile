PROJECT = safetyvalve

ERLC_OPTS = +debug_info '+{parse_transform, lager_transform}'

DEPS = lager
dep_lager = http://github.com/basho/lager.git 2.0.0

include erlang.mk

