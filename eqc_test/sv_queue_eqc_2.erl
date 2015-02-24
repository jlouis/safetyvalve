%% THE SIMPLEST POSSIBLE QUEUE
%% 
%% This module apapts the old queue test into a new queue test where we use the new
%% blocking features of Erlang QuickCheck to handle the queue behaviour.
%%
%% This crux of the problem is to handle the queue as we go along, by having a simpler
%% model of the queue and what the queue is doing.
%%
%% Here is the remarkable insight: we only need to track queue sizes in the model as a
%% start. The size of the queue and the current number of executing workers and the
%% current number of tokens in the bucket, encodes a model which has to run correctly
%%  w.r.t the underlying sv_queue implementation.
%%
%% In the following, we have a triple {C, Sz, T}, where C is the current number of
%% concurrent workers, Sz is the size of the queue, and T is the token count. There are
%% 10 rules for handling such a triple, depending on the possible cases for the system.
%% The following Handles these possibilities one by one, and documents the cases one
%% by one. In the simplied model, we assume a constant token fill-rate of 1, though in
%% practice this number can be any positive rate.
%%
-module(sv_queue_eqc_2).
-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

%% System state
-record(state, {
	concurrency = [],
	queue = [],
	tokens,
	max_concurrency,
	max_queue_size,
	max_tokens,
	rate
}).

-define(Q, test_queue_1).

%% Initial queue state generation
%% ----------------------------------------------------------------------
initial_state() ->
    ?LET({Rate, MaxT}, {choose(1,3), choose(1,3)},
      #state {
        concurrency = [],
        queue = [],
        tokens = min(Rate, MaxT),
        max_concurrency = choose(1,3),
        max_queue_size = choose(1,3),
        max_tokens = MaxT,
        rate = 1 }).

%% REPLENISHING TOKENS
%% ----------------------------------------------------------------------
replenish() ->
    ok = sv_queue:replenish(?Q),
    sv_queue:q(?Q, tokens).

%%% Case 1: replenishing the queue, when the token bucket is full,
%%		{C, Sz, MaxT} → poll → {C, Sz, MaxT}
%% The first case is a no–op since adding more tokens to an already full token bucket
%% has no effect at all
%%% Case 2a: replenishing the queue, when the queue is empty,
%%		{C, 0, T} when T < MaxT → poll → {C, 0, T+1}
%%% Case 2b: replenishing the queue, when max concurrency coutn reached,
%%		{MaxC, K, T} when T < MaxT → poll → {MaxC, K, T+1}
%% In this case, we add a new token to the bucket, but there is no work to be executed,
%% so the token is put into the reserve.
%%% Case 3: replenish the queue, when the queue is not empty,
%%		{C, K, 0} when C < MaxC, K > 0
%%			→ poll
%%			→ {C+1, K-1, 0}
%% We can move a queued task into the set of executing tasks. The token bucket remains
%% empty in this case, though in the general situation of a variable Rate, we have to do
%% more calculations in order to handle the case correctly.
replenish_args(_S) -> [].

replenish_next(#state {
	concurrency = Concurrency,
	queue = Q,
	tokens = T,
	max_concurrency = MaxC,
	max_tokens = MaxT,
	rate = Rate } = S, _, _) ->
    BucketBound = min(T+Rate, MaxT),
    case {length(Concurrency), length(Q), T} of
        {_, _, MaxT} -> S;
        {_, 0, T} when T < MaxT -> S#state { tokens = BucketBound };
        {MaxC, _, T} when T < MaxT -> S#state { tokens = BucketBound };
        {C, K, 0} when C < MaxC, K > 0 ->
          Workers = lists:min([K, MaxC - C, BucketBound]),
          {NewTasks, RestQueue} = lists:split(Workers, Q),
          S#state {
            concurrency = Concurrency ++ lists:reverse(NewTasks),
            queue = RestQueue,
            tokens = BucketBound - Workers }
    end.

replenish_callouts(#state {}, _) ->
    ?EMPTY.

replenish_features(#state {
	concurrency = Conc,
	queue = Q,
	tokens = T,
	max_tokens = MaxT,
	max_concurrency = MaxC}, _, _) ->
    case {length(Conc), length(Q), T} of
      {_, _, MaxT} -> ["R001: Replenish full bucket"];
      {_, 0, T} when T < MaxT -> ["R002a: Replenish when empty queue"];
      {MaxC, _, T} when T < MaxT -> ["R002b: Replenish with full concurrency count"];
      {C, K, 0} when C < MaxC, K > 0 -> ["R003: Replenish with workers ready"]
    end.

replenish_post(#state {
	concurrency = Concurrency,
	queue = Q,
	tokens = T,
	max_concurrency = MaxC,
	max_tokens = MaxT,
	rate = Rate}, [], Res) ->
    BucketBound = min(T+Rate, MaxT),
    QS = length(Q),
    Workers = lists:min([QS, MaxC - length(Concurrency), BucketBound]),
    case {length(Concurrency), QS, T, Res} of
        {_, _, MaxT, MaxT} -> true;
        {_, 0, T, BucketBound} when T < MaxT -> true;
        {MaxC, _, T, BucketBound} when T < MaxT -> true;
        {C, K, 0, R} when K > 0, C < MaxC, R == BucketBound - Workers -> true;
        _ -> {error, {replenish, Res}}
    end.

%% ENQUEUEING
%% ----------------------------------------------------------------------
enqueue() ->
    sv:ask(?Q, sv:timestamp()).

enqueue_args(_S) -> [].

%%% Case 4: Queue on a full queue
%%		{C, MaxQ, T} → queue → {C, MaxQ, T} (deny request)
%%		{C, K, 1} when K > 0 → impossible case—should immediately go to {C+1, K-1, 0}
%%			(??? This is a wierd case!)
%% If the queue is full, no change happens and the request is denied right away.
%%% Case 5: Queue with no tokens
%%		{C, K, 0} when K < MaxQ → queue → {C, K+1, 0}
%% Enqueueing without having tokens means the work goes into the queue.
%%% Case 6: Queue directly into work
%%		{C, 0, T} when C < MaxC, T > 0 → queue → {C+1, 0, T-1}
%% The free concurrency slot, means the task skips the queue entirely and moves directly
%% into the set of workers, eating up a token in the process.
%%% Case 7: Queue, waiting for a worker slot
%%		{MaxC, K, T} K < MaxQ, T > 0 → queue → {MaxC, K+1, T}
%% In this case, we have no slot, so we enqueue the work and wait for a worker slot to become
%% free.
enqueue_next(#state {
	concurrency = Conc,
	queue = Q,
	tokens = T,
	max_queue_size = MaxQ,
	max_concurrency = MaxC } = S, V, _) ->
    case {length(Conc), length(Q), T} of
        {_, MaxQ, _} -> S;
        {_, K, 0} when K < MaxQ -> S;
        {C, 0, T} when C < MaxC, T > 0 ->
            S#state { concurrency = Conc ++ [V], tokens = T-1 };
        {MaxC, K, T} when K < MaxQ, T > 0 -> S
    end.

enqueue_callouts(_S, []) ->
    ?SEQ([
        ?APPLY(block, [enqueue])
    ]).

enqueue_features(#state {
	concurrency = Conc,
	queue = Q,
	tokens = T,
	max_queue_size = MaxQ,
	max_concurrency = MaxC }, _, _) ->
    case {Conc, length(Q), T} of
        {_, MaxQ, _} -> ["R004: Enqueue a full queue"];
        {_, K, 0} when K < MaxQ -> ["R005: Enqueue when there are no available tokens"];
        {C, 0, T} when C < MaxC, T > 0 -> ["R006: Queue directly into work"];
        {MaxC, K, T} when K < MaxQ, T > 0 -> ["R007: Queue into the queue"]
    end.

%% MARKING WORK AS DONE
%% ----------------------------------------------------------------------

%%% Case 8: Done, no more work to start
%%		{C, 0, T} when C > 0 → done {C-1, 0, T}
%% We are done with work, and there is no more work to start.
%%% Case 9: Done, no more tokens
%%		{C, K, 0} when C > 0 → done → {C-1, K, 0}
%% We are done, and we are out of tokens
%%% Case 10: Done, continue with work
%%		{C, K, T} when C > 0, K > 0, T > 0 → done → {C, K-1, T-1}
%% We are done, and there is more work to start and tokens ready.
done({go, Ref}) ->
    sv:done(Ref).

done_args(#state { concurrency = Workers }) ->
    [elements(Workers)].

%%% We can only mark work as done when there really is work in the queue somewhere
done_pre(#state { concurrency = C }) -> C /= [].

done_next(#state {
	concurrency = Concurrency,
	queue = Q,
	tokens = T } = S, _, [Ref]) ->
    case {length(Concurrency), length(Q), T} of
        {_C, 0, _} -> S#state { concurrency = Concurrency -- [Ref]};
        {_C, K, 0} when K > 0 -> S#state { concurrency = Concurrency -- [Ref]};
        {_C, K, T} when K > 0, T > 0 -> S#state { tokens = T-1 }
    end.

done_callouts(#state { queue = []}, [_]) ->
    ?EMPTY;
done_callouts(#state { queue = [Next | _]}, [_]) ->
    ?SEQ([
        ?APPLY(unblock, [Next])
    ]).

done_features(#state {
	concurrency = Concurrency,
	queue = Q,
	tokens = T }, _, _) ->
    case {length(Concurrency), length(Q), T} of
       {_C, 0, _} -> ["R008: Done, with no more work in the queue"];
       {_C, K, 0} when K > 0 -> ["R009: Done, with no more tokens"];
       {_C, K, T} when K > 0, T > 0 -> ["R010: Done, continue with next work task"]
    end.

%% BLOCKING CALLS
%% ---------------------------------------------------------------------
block_callouts(S, [Op]) ->
    case nonblocking(S, Op) of
        true -> ?EMPTY;
        false ->
          ?SEQ([
              ?APPLY(add_blocked, [?SELF]),
              ?BLOCK,
              ?APPLY(del_blocked, [?SELF])
          ])
    end.

unblock_callouts(_S, [Pid]) ->
    ?SEQ([?UNBLOCK(Pid, ok)]).

add_blocked_next(S, _V, [Pid]) ->
    S#state { queue = S#state.queue ++ [Pid] }.

del_blocked_next(S, _V, [Pid]) ->
    S#state { queue = lists:keydelete(Pid, 1, S#state.queue) }.

%% NONBLOCKING
%% ---------------------------------------------------------------------

%% Nonblocking determines when an enqueue operation would block by analyzing the
%% Current state of the model. It returns true if the state is nonblocking, false otherwise.
nonblocking(# state {
	concurrency = Conc,
	queue = Q,
	tokens = T,
	max_queue_size = MaxQ,
	max_concurrency = MaxC }, enqueue) ->
  case {length(Conc), length(Q), T} of
      {_, MaxQ, _} -> true;
      {_, K, 0} when K < MaxQ -> false;
      {C, 0, T} when C < MaxC, T > 0 -> true;
      {MaxC, K, T} when K < MaxQ, T > 0 -> false
  end.

%% PROPERTIES
%% ----------------------------------------------------------------------

set_queue(
        #state {
          max_queue_size = MaxQ,
          max_concurrency = MaxC,
          max_tokens = MaxT,
          rate = Rate }) ->
    ok = application:set_env(safetyvalve, queues,
                             [{test_queue_1, [{hz, undefined},
                                              {rate, Rate},
                                              {token_limit, MaxT},
                                              {size, MaxQ},
                                              {concurrency, MaxC}
                                             ]}]).

prop_model_seq() ->
    ?FORALL(InitState, initial_state(),
      ?FORALL(Cmds, commands(?MODULE, InitState),
        ?TIMEOUT(400,
        ?TRAPEXIT(
          begin
            set_queue(InitState),
            {ok, _Started} = application:ensure_all_started(safetyvalve),
            {H,S,R} = run_commands(?MODULE, Cmds),
            ok = application:stop(safetyvalve),
            aggregate(command_names(Cmds),
              aggregate(eqc_statem:call_features(H),
                pretty_commands(?MODULE, Cmds, {H,S,R}, R == ok)))
          end)))).
