%% THE SIMPLEST POSSIBLE CASE
%%
%% When writing EQC test cases, begin by thinking in *microscopic*
%% test cases. That is, go for the smallest possible test case first
%% and then extend it. In our case, we have an extremely degenerate queue:
%%
%% * The concurrency level on the queue is 1.
%% * The queue size is 1, so there are at most a single waiter.
%% * The poll rate of the queue is 1 and maximum token count is 1.
%%
%% The postconditions we want to check are:
%% * The concurrency level in the SUT is *never* more than 1.
%% * The queue size in the SUT is *never* more than 1.
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

%% 1. Poll when full
%% {x, y, 1} -> poll -> {x, y, 1}

%% 2. poll, no queue ready
%% {x, 0, 0} -> poll -> {x, 0, 1}

%% 3. poll, queue ready
%% {0, K, 0} when K > 0 -> poll-> {1, K-1, 0}

%% 4. Full queue cases
%% {x, K, y} when K == MaxQ -> queue -> {x, K, y} (denied)
%% {0, K, 1} when K > 0 -> *impossible* - should immediately go to {1, 0, 0}

%% 5. Queue, no tokens
%% {1, K, 0} when K < MaxQ -> queue -> {1, K+1, 0}
%% {0, K, 0} when K < MaxQ -> queue -> {0, K+1, 0}

%% 6. Queue, to work
%% {0, 0, 1} -> queue -> {1, 0, 0}

%% 7. Queue, wait for worker
%% {1, K, 1} when K < MaxQ -> queue -> {1, K+1, 1}

%% 8. Done - no more work
%% {1, 0, x} -> done -> {0, 0, x}

%% 9. Done - no more tokens
%% {1, K, 0} -> done -> {0, K, 0}

%% 10. Done - with tokens
%% {1, K, 1} -> done -> {1, K-1, 0}

%% All in all, there are 10 possible transition commands available to
%% us when we are testing this. 
-record(state,
        { concurrency,
          queue_size,
          tokens,
          max_queue_size
        }).

-define(Q, test_queue_1).

%% The intial queue state
%% ----------------------------------------------------------------------
gen_initial_state() ->
    #state { concurrency = 0,
             queue_size  = 0,
             tokens      = 1,
             max_queue_size = choose(2, 5)
           }.

%% POLLING OF THE QUEUE
%% ----------------------------------------------------------------------
poll() ->
    sv_queue:poll(?Q),
    timer:sleep(1),
    eqc_helpers:fixpoint([whereis(?Q)]),
    sv_queue:q(?Q, tokens).

%%%% Case 1: polling the queue, when the token bucket is full
%%%% Case 2: polling the queue, when there is no-one queued
%%%% Case 3: polling the queue, when there is a waiter and no-one working
poll_command(_S) ->
    {call, ?MODULE, poll, []}.

poll_next(#state { concurrency = C, queue_size = QS, tokens = T } = S, _, _) ->
    case {C, QS, T} of
        {_, _, 1} -> S;
        {1, 1, 0} -> S#state { tokens = 1 };
        {_, 0, 0} -> S#state { tokens = 1 };
        {0, 1, 0} -> S#state { concurrency = 1,
                               queue_size = 0,
                               tokens = 0 }
    end.

poll_post(#state { concurrency = C, queue_size = QS, tokens = T}, _, Res) ->
    case {C, QS, T, Res} of
        {_, _, 1, 1} -> true;
        {1, 1, 0, 1} -> true;
        {_, 0, 0, 1} -> true;
        {0, 1, 0, 0} -> true;
        _ -> {error, {poll, Res}}
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

enqueue_next(#state { concurrency = C, queue_size = QS, tokens = T } = S, _, _) ->
    case {C, QS, T} of
        {_, 1, _} -> S;
        {_, 0, 0} -> S#state { queue_size = 1 };
        {0, 0, 1} -> S#state { concurrency = 1, queue_size = 0, tokens = 0 };
        {1, 0, 1} -> S#state { queue_size = 1 }
    end.

enqueue_post(#state { concurrency = C, queue_size = QS, tokens = T }, [], R) ->
    case {C, QS, T, R} of
        {_, 1, _, {{res, {error, queue_full}}, _}} -> true;
        {_, 0, 0, {queueing, 0}} -> true;
        {0, 0, 1, {{working, _}, 0}} -> true;
        {1, 0, 1, {queueing, 1}} -> true;
        _ -> {error, {enqueue_to_wait, R}}
    end.

%% MARKING WORK AS DONE
%% ----------------------------------------------------------------------
done() ->
    case manager:mark_done() of
        {ok, Pid} ->
            timer:sleep(1),
            eqc_helpers:fixpoint([whereis(manager), whereis(?Q) | manager:current_pids()]),
            manager:read_status(Pid);
        {error, none_working} ->
            error_logger:info_report([process_info(whereis(manager))]),
            {error, none_working}
    end.

%%%% Case 8: Done no more work
done_no_work() -> done().

done_no_work_command(_S) ->
    {call, ?MODULE, done_no_work, []}.

done_no_work_pre(#state { concurrency = 1, queue_size = 0 }) -> true;
done_no_work_pre(_) -> false.

done_no_work_next(S, _, _) -> S#state { concurrency = 0 }.

done_no_work_post(_S, [], {res, done}) -> true;
done_no_work_post(_S, [], Res) -> {error, {done_no_work, Res}}.

%%%% Case 9: Done, no more tokens
done_no_tokens() -> done().

done_no_tokens_command(_S) ->
    {call, ?MODULE, done_no_tokens, []}.

done_no_tokens_pre(#state { concurrency = 1, queue_size = 1, tokens = 0 }) ->
    true;
done_no_tokens_pre(_) -> false.

done_no_tokens_next(S, _, _) -> S#state { concurrency = 0 }.

done_no_tokens_post(_S, [], {res, done}) -> true;
done_no_tokens_post(_S, [], Res) -> {error, {done_no_tokens, Res}}.

%%%% Case 10: Done, run next
done_go_on() -> done().

done_go_on_command(_S) ->
    {call, ?MODULE, done_go_on, []}.

done_go_on_pre(#state { concurrency = 1, queue_size = 1, tokens = 1 }) ->
    true;
done_go_on_pre(_) -> false.

done_go_on_next(S, _, _) ->
    S#state { queue_size = 0, tokens = 0 }.

%% WEIGHTS
%% ----------------------------------------------------------------------
weight(#state { tokens = 1 }, poll) -> 100;
weight(#state { tokens = 0, queue_size = 0 }, poll) -> 100;
weight(#state { tokens = 0, queue_size = 1, concurrency = 0}, poll) -> 150;
weight(_S, poll) -> 100;
weight(#state { concurrency = C, queue_size = QS, tokens = T }, enqueue) ->
    case {C, QS, T} of
        {_, 1, _} -> 100;
        {_, 0, 0} -> 80;
        {0, 0, 1} -> 100;
        {1, 0, 1} -> 800
    end;
weight(_S, done_no_work)      -> 100;
weight(_S, done_no_tokens)    -> 800;
weight(_S, done_go_on)        -> 1500.

%% PROPERTIES
%% ----------------------------------------------------------------------

%% Check that the model can run
prop_model() ->
    ?FORALL(InitState, gen_initial_state(),
            ?FORALL(Cmds, commands(?MODULE, InitState),
                    ?TRAPEXIT(
                       begin
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
    eqc:module({numtests, 300}, ?MODULE).
