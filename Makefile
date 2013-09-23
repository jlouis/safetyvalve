REPO=safetyvalve

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

## DIALYZER
## ----------------------------------------------------
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
REPO = safetyvalve
COMBO_PLT = $(HOME)/.$(REPO)_combo_dialyzer_plt

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/lager/ebin

dialyzer:
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer --fullpath --plt $(COMBO_PLT) deps/*/ebin ebin | \
	    fgrep -v -f ./dialyzer.ignore-warnings

cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)
