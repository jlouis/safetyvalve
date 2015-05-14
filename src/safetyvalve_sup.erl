
-module(safetyvalve_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_queue/2, stop_queue/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_queue(Queue, Conf) -> {ok, pid()} | {error, invalid_configuration}
        when
            Queue :: undefined | atom(),
             Conf :: proplists:proplist().
start_queue(Queue, Conf) ->
    C = sv_queue:parse_configuration(Conf),
    case supervisor:start_child(?MODULE, [Queue, C]) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} ->
            %% a process is already running registered with the
            %% provided name, we must now obtain it's current
            %% configuration and check that it is indeed the same
            case sv_queue:q(Pid, configuration) of
                C -> {ok, Pid};
                _ -> {error, invalid_configuration}
            end
    end.

-spec stop_queue(Queue) -> ok | {error, not_found | simple_one_for_one}
    when
        Queue :: undefined | atom().
stop_queue(Queue) when is_pid(Queue) ->
    supervisor:terminate_child(?MODULE, Queue);
stop_queue(Queue) when is_atom(Queue) ->
    supervisor:terminate_child(?MODULE, whereis(Queue)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 3, 600},
        [{queue, {sv_queue, start_link, []},
          transient, 5000, worker, [sv_queue]}] } }.
