-module(t).

-export([t/0]).

t() ->
    application:start(syntax_tools),
    application:start(compiler),
    application:start(lager),
    application:load(safetyvalve),
    eqc:module({numtests, 500}, sv_queue_eqc).