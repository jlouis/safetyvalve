
-module(safetyvalve_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_queue/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(QCHILD(I, C, Type), {I, {sv_queue, start_link, [I, C]},
                             permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_queue(Name, Conf) ->
    C = sv_queue:parse_configuration(Conf),
    supervisor:start_child(?MODULE,
                           ?QCHILD(Name, C, worker)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 3, 600}, []} }.

