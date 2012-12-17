-module(sv_codel_eqc).

-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-eqc_group_commands(true).

-record(state,
    { ttd = 10,
      t = 0
    }).
    
%% ADVANCING THE TIME
%% ----------------------------------------------------------------------

advance_time(_Step) -> ok.

advance_time_command(#state { ttd = TTD}) ->
	{call, ?MODULE, advance_time, [choose(0, TTD)]}.

advance_time_pre(#state { ttd = TTD }) when TTD > 0 -> true;
advance_time_pre(_S) -> false.

advance_time_next(#state { t = T, ttd = TTD } = State, _, [Step]) ->
	State#state { t = T + Step, ttd = TTD - Step}.

dequeue() -> ok.

dequeue_command(_S) ->
	{call, ?MODULE, dequeue, []}.
	
dequeue_pre(#state { ttd = 0 }) -> true;
dequeue_pre(_S) -> false.

dequeue_next(State, _, []) ->
	State#state { ttd = 10 }.
	
prop_model() ->
	?FORALL(Cmds, commands(?MODULE, #state{}),
		?TRAPEXIT(
			begin
				{H, S, R} = run_commands(?MODULE, Cmds),
				?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                               [History, State, Result]),
                                     aggregate(command_names(Cmds), Result =:= ok))
                       end)
                   )
           ).

