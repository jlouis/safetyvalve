-module(ws).

-export([t/0]).

t() ->
  application:start(crypto),
  application:start(ranch),
  application:start(cowboy),
  application:start(lager),
  application:start(syntax_tools),
  application:start(compiler),
  application:start(lager),
  ok = application:start(safetyvalve),
  
  start_cowboy(),
  sv_tracer:start_link("trace.out").
  
start_cowboy() ->
  Dispatch = [{'_', [{'_', ws_handler, []}]}],
  cowboy:start_http(ws_listener, 100, [{port, 8080}], [{dispatch, Dispatch}]).
