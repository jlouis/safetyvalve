-module(sv_codel_eqc).

-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").

-record(model,
    { t = 30000, st }).

g_sv_codel_args() ->
    ?LET(T, choose(5, 50),
       [T, choose(T, 200)]).

g_cmd_advance_time(M) ->
    {call, ?MODULE, advance_time, [M, g_time_advance()]}.

g_time_advance() ->
    choose(1, 1000).
    
g_model(0, todo) ->
	oneof([{call, ?MODULE, new, g_sv_codel_args()}]);
g_model(N, todo) ->
	frequency([
		{1, g_model(0, todo)},
		{N, ?LET(M, g_model(max(0, N-2), todo),
		    frequency(
		        [{100, g_cmd_advance_time(M)}] ++
		        [{100, {call, ?MODULE, enqueue, [M]}}] ++
		        [{100, {call, ?MODULE, dequeue, [M]}}]))}]).
		        

g_model() ->
    ?SIZED(Size, g_model(Size, todo)).

%% Properties
%% ----------------------------------------------

%% Verify that the queue runs if we blindly execute it
xprop_termination() ->
    ?FORALL(M, g_model(),
    	begin
    		_R = eval(M),
    		true
    	end).

%% Various observations on a CoDel queue
prop_observations() ->
    ?FORALL(M, g_model(),
	begin
            #model { t = T, st = ST} = eval(M),
            case sv_codel:dequeue(T+1, ST) of
                {empty, _Dropped, EmptyState} ->
                    verify_empty(EmptyState);
                {drop, [_Pkt], _CoDelState} ->
                    classify(true, start_drop, true);
                {_Pkt, [_ | _], CoDelState} ->
                    verify_dropped(CoDelState);
                {_Pkt, _Dropped, _SomeState} ->
                    classify(true, dequeue, true)
             end
        	end).


verify_dropped(CoDelState) ->
    %% We dropped packets, our state must be dropping
    PL = sv_codel:qstate(CoDelState),
    classify(true, dropped,
        case proplists:get_value(dropping, PL) of
          true -> true;
          false ->
            case queue:is_empty(proplists:get_value(queue, PL)) of
              true -> true;
              false -> {error, {no_drop, PL}}
            end
        end).

verify_empty(EmptyState) ->
    %% Empty queues are never dropping and they reset first-above-time
    PL = sv_codel:qstate(EmptyState),
    classify(true, empty_queue,
        case proplists:get_value(dropping, PL) of
            false ->
                case proplists:get_value(first_above_time, PL) of
                   0 -> true;
                   K -> {error, {fat_not_zero, K, PL}}
                 end;
            true ->
                {error, {empty_and_dropping, PL}}
        end).

%% Operations
%% ----------------------------------------------

new(Target, Interval) ->
	#model { t = 0, st = sv_codel:init(Target, Interval) }.

advance_time(#model { t = T } = State, K) ->
    State#model { t = T + K  }.

enqueue(#model { t = T, st = ST } = State) ->
	State#model { t = T+1, st = sv_codel:enqueue({pkt, T}, T, ST) }.
	
dequeue(#model { t = T, st = ST } = State) ->
    ST2 =
    	case sv_codel:dequeue(T, ST) of
    	    {_, _, S} -> S
    	end,
    State#model { t = T+1, st = ST2 }.
