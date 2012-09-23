%% THE SIMPLEST POSSIBLE CASE
%%
%% When writing EQC test cases, begin by thinking in *microscopic*
%% test cases. That is, go for the smallest possible test case first
%% and then extend it. In our case, we have an extremely degenerate queue:
%%
%% * The concurrency level on the queue is 1.
%% * The queue size is K, so there are between 0 and K workers waiting
%%   in the queue
%% * The poll rate of the queue is 1 and maximum token count is 1.
%%
%% The postconditions we want to check are:
%% * The concurrency level in the SUT is *never* more than 1.
%% * The queue size in the SUT is *never* more than K.
%% * The maximum token count is *never* more than 1.
%%
%% So if we spawn a new process when the queue is full, we expect that
%% new spawn to be denied queueing access since the queue is overloaded.
%%
%% We *do* want to generate random command sequences for our queue to
%% check this however, hence we write a quickcheck test case for it.
%%
-module(sv_queue_eqc).

-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-eqc_group_commands(true).

%% The record state are 0/1 or 0/K values on concurrency, queue size and
%% tokens. These mandate when you can expect a certain command to be possible
%% and also captures the possible transition states on the queue:

%% 1. Poll when full         : {x, y, MaxT} -> poll -> {x, y, MaxT}
%% 2. poll, no queue ready   : {x, 0, T} when T < MaxT -> poll -> {x, 0, T+1}
%% 3. poll, queue ready      : {C, K, T} when K > 0, C < MaxC, T < MaxT -> poll -> {C+1, K-1, T+1}
%% 4. Full queue cases       : {x, K, y} when K == MaxQ -> queue -> {x, K, y} (denied)
%%                           : {C, K, 1} when K > 0 -> *impossible* - should immediately go to {C+1, 0, 0}
%% 5. Queue, no tokens       : {C, K, 0} when K < MaxQ -> queue -> {C, K+1, 0}
%% 6. Queue, to work         : {C, 0, T} when C < MaxC, T > 0 -> queue -> {C+1, 0, T-1}
%% 7. Queue, wait for worker : {C, K, T} when K < MaxQ, C == MaxC, T > 0 -> queue -> {MaxC, K+1, T}
%% 8. Done - no more work    : {C, 0, x} when C > 0 -> done -> {C-1, 0, x}
%% 9. Done - no more tokens  : {C, K, 0} when C > 0 -> done -> {C-1, K, 0}
%% 10. Done - with tokens    : {C, K, T} when C > 0, K > 0, T > 0 -> done -> {C, K-1, T-1}

%% All in all, there are 10 possible transition commands available to
%% us when we are testing this. These can be coalesced by considering
%% each of the three possible commands you can execute: poll, queue
%% and done.
-record(state,
        { concurrency,
          queue_size,
          tokens,
          max_concurrency,
          max_queue_size,
          max_tokens
        }).

-define(Q, test_queue_1).

%% The intial queue state
%% ----------------------------------------------------------------------
gen_initial_state() ->
    #state {
      concurrency = 0,
      queue_size  = 0,
      tokens      = 1,
      max_concurrency = choose(1,5),
      max_queue_size = choose(1,5),
      max_tokens  = choose(1,5),
      replenish_rate = 1
    }.

%% POLLING OF THE QUEUE
%% ----------------------------------------------------------------------
replenish() ->
    sv_queue:poll(?Q),
    timer:sleep(1),
    eqc_helpers:fixpoint([whereis(?Q)]),
    sv_queue:q(?Q, tokens).

%%%% Case 1: replenishing the queue, when the token bucket is full
%%%% Case 2: replenishing the queue, when there is no-one queued
%%%% Case 3: replenishing the queue, when there is a waiter and no-one working
replenish_command(_S) ->
    {call, ?MODULE, replenish, []}.

replenish_next(#state { concurrency = Conc,
                   queue_size = QS,
                   tokens = T,
                   max_concurrency = MaxC,
                   max_tokens = MaxT,
                   replenish_rate = Rate } = S, _, _) ->
    BucketCount = min(T+Rate, MaxT),
    case {Conc, QS, T} of
        %% Token bucket is full
        {_, _, MaxT} -> S;
        %% Nothing to dequeue
        {_, 0, T} when T < MaxT -> S#state { tokens = BucketCount };
        %% Concurrency count full
        {MaxC, _, T} when T < MaxT -> S#state { tokens = BucketCount };
        %% Add work to the queue code
        {C, K, 0} when K > 0, C < MaxC -> S#state { concurrency = C+1,
                                                    queue_size = K-1,
                                                    tokens = 0 }
    end.

replenish_post(#state { concurrency = Conc,
                   queue_size = QS,
                   tokens = T,
                   max_concurrency = MaxC,
                   max_tokens = MaxT,
                   replenish_rate = Rate
                 }, _, Res) ->
    BucketCount = min(T+Rate, MaxT),
    case {Conc, QS, T, Res} of
        {_,    _, MaxT, MaxT} -> true;
        {_,    0, T,    BucketCount} when T < MaxT -> true;
        {MaxC, _, T,    BucketCount} when T < MaxT -> true;
        {C,    K, 0,    0} when K > 0, C < MaxC -> true;
        _ -> {error, {replenish, Res}}
    end.

%% ENQUEUEING
%% ----------------------------------------------------------------------

%% The queueing command is generic. It does the same thing: spawn a
%% new worker, wait until the system reaches a fixpoint and then read
%% out the workers status.

%%%% Case 4: Enqueueing on a full queue
%%%% Case 5: Enqueuing when there is no available token
%%%% Case 6: Enqueueing when there is a token and no worker
%%%% Case 7: Enqueueing when there is a worker
enqueue() ->
    {ok, Pid} = manager:spawn_worker(),
    timer:sleep(1),
    eqc_helpers:fixpoint([whereis(manager) , whereis(?Q) | manager:current_pids()]),
    {manager:read_status(Pid), sv_queue:q(?Q, tokens)}.

enqueue_command(_S) ->
    {call, ?MODULE, enqueue, []}.

enqueue_next(#state { concurrency = Conc, queue_size = QS, tokens = T,
                      max_queue_size = MaxQ,
                      max_concurrency = MaxC} = S, _, _) ->
    case {Conc, QS, T} of
        {_, K, _} when K == MaxQ -> S;
        {_, K, 0} when K <  MaxQ -> S#state { queue_size = K+1 };
        {C, 0, T} when C < MaxC, T > 0 -> S#state { concurrency = C+1,
                                                    queue_size = 0,
                                                    tokens = T-1 };
        {MaxC, K, T} when K < MaxQ, T > 0 -> S#state { queue_size = K+1 }
    end.

enqueue_post(#state { concurrency = Conc, queue_size = QS, tokens = T,
                      max_queue_size = MaxQ,
                      max_concurrency = MaxC }, [], R) ->
    case {Conc, QS, T, R} of
        {_, K, _, {{res, {error, queue_full}}, _}} when K == MaxQ -> true;
        {_, K, 0, {queueing, 0}} when K < MaxQ -> true;
        {C, 0, T, {{working, _}, Ret}}
          when C < MaxC, T > 0,
               Ret == T-1 -> true;
        {MaxC, K, T, {queueing, T}} when K < MaxQ, T > 0 -> true;
        _ -> {error, {enqueue, R}}
    end.

%% MARKING WORK AS DONE
%% ----------------------------------------------------------------------

%%%% Case 8: Done no more work
%%%% Case 9: Done, no more tokens
%%%% Case 10: Done, run next
done() ->
    case manager:mark_done() of
        {ok, Pid} ->
            timer:sleep(1),
            eqc_helpers:fixpoint([whereis(manager), whereis(?Q) | manager:current_pids()]),
            {manager:read_status(Pid), sv_queue:q(?Q, tokens)};
        {error, none_working} ->
            error_logger:info_report([process_info(whereis(manager))]),
            {error, none_working}
    end.

done_command(_S) ->
    {call, ?MODULE, done, []}.

done_pre(#state { concurrency = C }) when C > 0 -> true;
done_pre(_) -> false.


%% TODO: The when C > 0's here are really redundant since the
%% precondition filters out any problem.
done_next(#state { concurrency = C,
                   queue_size = QS,
                   tokens = T } = S, _, _) ->
    case {C, QS, T} of
        %% Done, but no-one in the queue waits
        {C, 0, _} when C > 0 -> S#state { concurrency = C-1 };
        %% Done, but no tokens are available
        {C, K, 0} when C > 0, K > 0 -> S#state { concurrency = C-1 };
        %% Done, and we can start next job
        {C, K, T}
          when C > 0,
               K > 0,
               T > 0 -> S#state { queue_size = K-1, tokens = T-1 }
    end.

done_post(#state { concurrency = C, queue_size = QS, tokens = T }, _, Res) ->
    case {C, QS, T, Res} of
        {C, 0, _, {{res, done}, T}}
          when C > 0        -> true;
        {C, K, 0, {{res, done}, 0}}
          when C > 0, K > 0 -> true;
        {C, K, T, {{res, done}, R}}
          when C > 0, K > 0, T > 0,
               R == T-1 -> true;
        R -> {error, {done, R}}
    end.

%% WEIGHTS
%% ----------------------------------------------------------------------

weight(#state { concurrency = C, queue_size = QS, tokens = T,
                max_tokens = MaxT }, replenish) ->
    case {C, QS, T} of
        {_, _, MaxT} -> 100;
        {_, 0, T} when T > 0 -> 100;
        {0, K, 0} when K > 0 -> 150;
        _         -> 100
    end;
weight(#state { concurrency = C, queue_size = QS, tokens = T }, enqueue) ->
    case {C, QS, T} of
        {_, K, _} when K > 0 -> 100;
        {_, 0, 0} -> 80;
        {0, 0, T} when T > 0 -> 100;
        {C, 0, T} when C > 0, T > 0 -> 800
    end;
weight(#state { concurrency = C, queue_size = QS, tokens = T }, done) ->
    case {C, QS, T} of
        {_, 0, _} -> 100;
        {_, K, 0} when K > 0 -> 800;
        {_, K, T} when K > 0, T > 0 -> 1500
    end.

%% PROPERTIES
%% ----------------------------------------------------------------------

set_queue(#state { max_queue_size = MaxQ,
                   max_concurrency = MaxC,
                   max_tokens = MaxT
                 }) ->
    ok = application:set_env(safetyvalve, queues,
                             [{test_queue_1, [{hz, undefined},
                                              {rate, 1},
                                              {token_limit, MaxT},
                                              {size, MaxQ},
                                              {concurrency, MaxC}
                                             ]}]).

%% Check that the model can run
prop_model() ->
    ?FORALL(InitState, gen_initial_state(),
            ?FORALL(Cmds, commands(?MODULE, InitState),
                    ?TRAPEXIT(
                       begin
                           set_queue(InitState),
                           {ok, _Pid} = manager:start(),
                           application:start(safetyvalve),
                           {History, State, Result} = run_commands(?MODULE, Cmds),
                           application:stop(safetyvalve),
                           ok = manager:stop(),
                           ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                               [History, State, Result]),
                                     aggregate(command_names(Cmds), Result =:= ok))
                       end)
                   )
           ).

t() ->
    application:start(syntax_tools),
    application:start(compiler),
    application:start(lager),
    application:load(safetyvalve),
    eqc:module({numtests, 500}, ?MODULE).
