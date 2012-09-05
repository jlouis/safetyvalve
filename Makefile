.PHONY: all compile deps clean test ct ct_setup

all:
	rebar compile

compile:
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

test: ct

ct_setup:
	mkdir -p log logs

ct: ct_setup compile
	erl -noshell \
            -pa deps/*/ebin -pa ebin \
	    -sname ct \
	    -env TEST_DIR test \
	    -spec ct/sv.spec \
	    -config ct/test.config \
            -dir test \
            -s ct_run script_start -s erlang halt

