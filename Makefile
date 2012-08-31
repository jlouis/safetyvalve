CT_RUN=ct_run
.PHONY: all deps clean test ct ct_setup

all:
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

test: ct

ct_setup:
	mkdir -p log logs

ct: ct_setup
	${CT_RUN} -pa ebin \
		  -pa deps/lager/ebin \
                  -spec ct/sv.spec \
		  -logdir log

