-module(sv_codel_eqc).

-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").

-record(model,
    { ttd = 10,
      t = 0,
      st
    }).
    
g_time_advance() ->
    choose(1, 10).
    
g_model(0, todo) ->
	oneof([{call, ?MODULE, new, []}]);
g_model(N, todo) ->
	frequency([
		{1, g_model(0, todo)},
		{N, ?LET(M, g_model(max(0, N-2), todo),
		    frequency(
		        [{400, {call, ?MODULE, advance_time,
		            [M, g_time_advance()]}} || not boundary(M)] ++
		        [{100, {call, ?MODULE, enqueue, [M]}}] ++
		        [{200, {call, ?MODULE, dequeue, [M]}} || boundary(M)]))}]).
		        

g_model() ->
    ?SIZED(Size, g_model(Size, todo)).

boundary(#model { ttd = 0 }) -> true;
boundary(_) -> false.

%% Properties
%% ----------------------------------------------

%% Verify that the queue runs if we blindly execute it
prop_termination() ->
    ?FORALL(M, g_model(),
    	begin
    		_R = eval(M),
    		true
    	end).

%% If the queue is empty, we are never in a dropping state
prop_empty_q_no_drop() ->
    ?FORALL(M, g_model(),
        begin
            #model { t = T, st = ST} = eval(M),
            case sv_codel:dequeue(T+1, ST) of
                {empty, _Dropped, EmptyState} ->
                    PL = sv_codel:qstate(EmptyState),
                    not proplists:get_value(dropping, PL)
                      andalso proplists:get_value(first_above_time, PL) == 0;
                {ok, _Pkt, _Dropped, _SomeState} ->
                    true
            end
        end).

%% Operations
%% ----------------------------------------------

new() ->
	#model { ttd = 10, t = 0, st = sv_codel:init() }.

advance_time(#model { t = T, ttd = TTD } = State, K) ->
    Inc = min(K, TTD),
    State#model { t = T + Inc, ttd = TTD - Inc }.

enqueue(#model { t = T, st = ST } = State) ->
	State#model { t = T+1, st = sv_codel:enqueue({pkt, T}, T, ST) }.
	
dequeue(#model { t = T, st = ST } = State) ->
	{_, ST2} = sv_codel:dequeue(T, ST),
	State#model { t = T+1, ttd = 10, st = ST2 }.
