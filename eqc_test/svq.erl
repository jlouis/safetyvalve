%% THE SIMPLEST POSSIBLE QUEUE
%% 
%% This module apapts the old queue test into a new queue test where we use the new
%% blocking features of Erlang QuickCheck to handle the queue behaviour.
%%
%% The key insight is that we can model a typical safetyvalve task as a process which may
%% block twice. First time because it gets queued, and the second time while it is doing
%% "work". We can model this in a state by having a queue of who is queued (the processes
%% currently 'asking') and a set of who is doing work ('working').
%%
%% Simple analysis of the model state determines when a process will block or not, by looking
%% at the representation of how many processes are asking and how many are doing work and
%% how many tokens we have left.
%%
%% The task can be described entirely by ?BLOCK/?UNBLOCK rules in the _callout/2 sections
%% of the commands.
%%
%% The remarkable result is an extremely simple model on one hand, but with a lot of power.
%% The simple specification provides a complete randomized run–time harness around the
%% SafetyValve system and checks it for correctness.
-module(svq).
-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-record(state, {
	working = [],
	asking = [],
	tokens,
	max_working,
	max_asking,
	max_tokens
}).

%% Queue name we are using throughout the test
-define(Q, test_svq).
-define(TIMEOUT, 200).

%% Initial queue state generation
%% ----------------------------------------------------------------------
state_range(N) -> choose(1, N).

initial_state() ->
    ?LET(MaxT, state_range(5),
      #state {
      	working = [], asking = [], tokens = MaxT,
      	max_working = state_range(5),
      	max_asking = state_range(5),
      	max_tokens = MaxT
    }).

%% TASKS
%% ----------------------------------------------------------------------
%%
%% A task is a typical task when using safetyvalve
task() ->
    case sv:ask(?Q, sv:timestamp()) of
        {error, Reason} ->
            {error, Reason};
        {go, Ref} ->
            start_work(),
            sv:done(?Q, Ref, sv:timestamp())
    end.

task_args(_S) -> [].

task_callouts(#state { asking = As, max_asking = MaxA } = S, []) ->
    case will_block(S, enqueue) of
      false ->
        case length(As) of
          MaxA -> ?RET({error, queue_full});
          K when K < MaxA ->
            ?MATCH(R, ?APPLY(run_task, [])),
            ?RET(R)
        end;
      true ->
        ?APPLY(add_asking, [?SELF]),
        ?BLOCK(?SELF),
        ?APPLY(del_asking, [?SELF]),
        ?MATCH(R, ?APPLY(run_task, [])),
        ?RET(R)
    end.
    
task_features(#state {
	working = Workers,
	asking = Q,
	tokens = T,
	max_asking = MaxQ,
	max_working = MaxC }, _, _) ->
    case {length(Workers), length(Q), T} of
        {_, MaxQ, _} -> ["R004: Enqueue a full queue"];
        {_, _, 0} -> ["R005: Enqueue when there are no available tokens"];
        {C, 0, _} when C < MaxC -> ["R006: Queue directly into work"];
        {MaxC, _, _} -> ["R007: Queue into the queue"]
    end.

%% MARKING WORK AS DONE
%% ----------------------------------------------------------------------

done(Pid) ->
    end_work(Pid).
    
done_args(#state { working = Workers }) ->
    [elements(Workers)].
    
done_pre(#state { working = Ws }) -> Ws /= [].

done_callouts(_S, [Pid]) ->
    ?UNBLOCK(Pid, ok),
    ?APPLY(unblock, []),
    ?RET(ok).

%% done_features(#state {
%% 	concurrency = Concurrency,
%% 	queue = Q,
%% 	tokens = T }, _, _) ->
%%     case {length(Concurrency), length(Q), T} of
%%        {_C, 0, _} -> ["R008: Done, with no more work in the queue"];
%%        {_C, K, 0} when K > 0 -> ["R009: Done, with no more tokens"];
%%        {_C, K, T} when K > 0, T > 0 -> ["R010: Done, continue with next work task"]
%%     end.

%% BLOCKING CALLS
%% ---------------------------------------------------------------------

unblock_callouts(S, []) ->
    case will_unblock(S, dequeue) of
        no -> ?EMPTY;
        {yes, Whom} ->
            ?UNBLOCK(Whom, ok)
    end.

add_asking_next(S, _V, [Pid]) ->
    S#state { asking = S#state.asking ++ [Pid] }.

del_asking_next(S, _V, [Pid]) ->
    S#state { asking = S#state.asking -- [Pid] }.

add_working_next(S, _V, [Pid]) ->
    S#state { working = S#state.working ++ [Pid], tokens = S#state.tokens - 1 }.
    
add_working_callouts(#state { tokens = Tokens }, [_Pid]) when Tokens > 0 ->
    ?EMPTY;
add_working_callouts(#state { }, [_Pid])  ->
    ?FAIL("Model failure, out of tokens, but adding work").

del_working_next(S, _V, [Pid]) ->
    S#state { working = S#state.working -- [Pid] }.
    
run_task_callouts(_S, []) ->
    ?APPLY(add_working, [?SELF]),
    ?BLOCK,
    ?APPLY(del_working, [?SELF]),
    ?RET(ok).

%% BLOCKING BEHAVIOUR
%% ---------------------------------------------------------------------

%% will_block/2 determines when an enqueue operation would block by analyzing the
%% Current state of the model. It returns true if the state is nonblocking, false otherwise.
will_block(#state {
	working = Workers,
	asking = Asking,
	tokens = Tokens,
	max_asking = MaxA,
	max_working = MaxWs }, enqueue) ->
  case {length(Workers), length(Asking), Tokens} of
      {_, MaxA, _} -> false;
      {_, _, 0} -> true;
      {MaxWs, _, _} -> true;
      {_, _, _} -> false
  end.
  
  
%% will_unblock/2 determines when a state is such that a process can be unblocked.
will_unblock(#state {
	working = Workers,
	asking = Asking,
	tokens = Tokens,
	max_working = MaxWs }, dequeue) ->
    case {length(Workers), Asking, Tokens} of
        {MaxWs, _, _} -> no;
        {_, [], _} -> no;
        {_, _, 0} -> no;
        {_, [Next|_], _} -> {yes, Next}
    end.

%% STARTING AND ENDING WORK
%% ----------------------------------------------------------------------
start_work() ->
    receive
        {end_task, From, Ref} ->
            From ! {end_ack, Ref},
            ok
    end.
    
end_work(Pid) ->
    Ref = make_ref(),
    Pid ! {end_task, self(), Ref},
    receive
        {end_ack, Ref} -> ok
    after ?TIMEOUT ->
        {error, timeout}
    end.

    
%% PROPERTIES
%% ----------------------------------------------------------------------
postcondition_common(S, Call, Res) ->
    eq(Res, return_value(S, Call)).

set_queue(
        #state {
          max_asking = MaxA,
          max_working = MaxWs,
          max_tokens = MaxT }) ->
    ok = application:set_env(safetyvalve, queues,
                             [{?Q, [{hz, undefined},
                                              {rate, 1},
                                              {token_limit, MaxT},
                                              {size, MaxA},
                                              {concurrency, MaxWs}
                                             ]}]).

setup() ->
    ok.
    
cleanup() ->
    (catch application:stop(safetyvalve)),
    {ok, _Started} = application:ensure_all_started(safetyvalve),
    ok.

prop_model_seq() ->
  ?SETUP(fun() ->
      setup(),
      fun() -> ok end
    end,
      ?FORALL(InitState, initial_state(),
      ?FORALL(Cmds, commands(?MODULE, InitState),
      ?TIMEOUT(400,
            begin
              set_queue(InitState),
              ok = cleanup(),
              {H,S,R} = run_commands(?MODULE, Cmds),
              aggregate(command_names(Cmds),
                  pretty_commands(?MODULE, Cmds, {H,S,R}, R == ok))
            end)))).
