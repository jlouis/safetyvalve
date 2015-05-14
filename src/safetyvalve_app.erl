-module(safetyvalve_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, MainPid} = safetyvalve_sup:start_link(),

    %% empty static queue definition is allowed
    Queues = case application:get_env(safetyvalve, queues, undefined) of
                undefined -> [];
                Qs -> Qs
             end,

    %% launch statically configured queues
    [{ok, _QueuePid} = sv:new(Name, Conf) || {Name, Conf} <- Queues],
    {ok, MainPid}.

stop(_State) ->
    ok.
