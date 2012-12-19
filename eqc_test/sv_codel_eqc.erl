-module(sv_codel_eqc).

-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").
-eqc_group_commands(true).

-record(state,
    { ttd = 10,
      t = 0,
      st
    }).
    
g_time_advance(#state { ttd = N, t = T } = State) ->
	?LET(K, choose(0, N),
		State#state { ttid = N - K, t = T + K }).
    
g_model(0, todo) ->
	oneof([{call, ?MODULE, new, []}]);
g_model(N, todo) ->
	frequency([
		{1, g_model(0, todo)},
		{N, ?LET(M, g_model(max(0, N-2), todo),
		    frequency(
		        [{200, {call, ?MODULE, advance_time, M, g_time_advance(M)}},
		         {200, {call, ?MODULE, enqueue, [M]}},
		         {100, {call, ?MODULE, dequeue, [M]}}
		        ])}]).

%% Operations
%% ----------------------------------------------

new() ->
	#state { ttd = 10, t = 0, st = sv_codel:init() }.

enqueue(State) -> State.
	
dequeue(State) -> State.
