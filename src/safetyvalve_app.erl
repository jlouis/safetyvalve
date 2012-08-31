-module(safetyvalve_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, MainPid} = safetyvalve_sup:start_link(),

    {ok, Queues} = application:get_env(safetyvalve, queues),
    
    [ok = start_queue(Q) || Q <- Queues],
    {ok, MainPid}.
    
stop(_State) ->
    ok.

%% ----------------------------------------------------------------------
start_queue({QName, Conf}) ->
    {ok, _Pid} = safetyvalve_sup:start_queue(QName, Conf),
    ok.
