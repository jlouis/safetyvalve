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

%% The record state are 0/1 values on concurrency, queue size and
%% tokens. These mandate when you can expect a certain command to be possible
%% and also captures the possible transition states on the queue:

%% 1. Poll when full
%% {x, y, 1} -> poll -> {x, y, 1}

%% 2. poll, no queue ready
%% {x, 0, 0} -> poll -> {x, 0, 1}

%% 3. poll, queue ready
%% {0, 1, 0} -> poll-> {1, 0, 0}

%% 4. Full queue cases
%% {x, 1, y} -> queue -> {x, 1, y} (denied)
%% {0, 1, 1} -> *impossible* - should immediately go to {1, 0, 0}

%% 5. Queue, no tokens
%% {1, 0, 0} -> queue -> {1, 1, 0}
%% {0, 0, 0} -> queue -> {0, 1, 0}

%% 6. Queue, to work
%% {0, 0, 1} -> queue -> {1, 0, 0}

%% 7. Queue, wait for worker
%% {1, 0, 1} -> queue -> {1, 1, 1}

%% 8. Done - no more work
%% {1, 0, x} -> done -> {0, 0, x}

%% 9. Done - no more tokens
%% {1, 1, 0} -> done -> {0, 1, 0}

%% 10. Done - with tokens
%% {1, 1, 1} -> done -> {1, 0, 0}

%% All in all, there are 10 possible transition commands available to
%% us when we are testing this. 
-record(state,
        { concurrency,
          queue_size,
          tokens }).

-define(Q, test_queue_1).

%% The intial queue state
%% ----------------------------------------------------------------------
initial_state() ->
    #state { concurrency = 0,
             queue_size  = 0,
             tokens      = 1 }. %% Initialized to the rate of the queue


%% POLLING OF THE QUEUE
%% ----------------------------------------------------------------------
poll() ->
    sv_queue:poll(?Q),
    sv_queue:q(?Q, tokens).

poll_full() -> poll().

%%%% Case 1: polling the queue, when the token bucket is full
poll_full_command(_S) ->
    {call, ?MODULE, poll_full, []}.

%% This case matches, when the token bucket is full
poll_full_pre(#state { tokens = T }) -> T == 1.

poll_full_next(S, _, _) -> S.

poll_full_post(_S, [], 1) -> true;
poll_full_post(_S, _, _) -> {error, wrong_token_count}.

%%%% Case 2: polling the queue, when there is no-one queued
poll_empty_q() -> poll().

poll_empty_q_command(_S) ->
    {call, ?MODULE, poll_empty_q, []}.

%% This case matches if we are lacking a token and the queue is empty
poll_empty_q_pre(#state { tokens = T, queue_size = QS }) ->
    T == 0 andalso QS == 0.

poll_empty_q_next(S, _, _) -> S#state { tokens = 1 }.

poll_empty_q_post(_S, [], 1) -> true;
poll_empty_q_post(_S, [], _) -> {error, poll_empty_q_post}.

%%%% Case 3: polling the queue, when there is a waiter and no-one working
poll_to_work() -> poll().

poll_to_work_command(_S) ->
    {call, ?MODULE, poll_to_work, []}.

poll_to_work_pre(#state { tokens = T, queue_size = QS, concurrency = C }) ->
    T == 0 andalso QS == 1 andalso C == 0.

poll_to_work_next(S, _, _) ->
    S#state { concurrency = 1, queue_size = 0, tokens = 0 }.

%% ENQUEUEING
%% ----------------------------------------------------------------------

%% The queueing command is generic. It does the same thing: spawn a
%% new worker, wait until the system reaches a fixpoint and then read
%% out the workers status.
enqueue() ->
    {ok, Pid} = manager:spawn_worker(),
    eqc_helpers:fixpoint([whereis(manager) | manager:current_pids()]),
    manager:read_status(Pid).

%%%% Case 4: Enqueueing on a full queue
enqueue_full() -> enqueue().

enqueue_full_command(_S) ->
    {call, ?MODULE, enqueue_full, []}.

enqueue_full_pre(#state { queue_size = QS }) -> QS == 1.

enqueue_full_next(S, _, _) -> S.

enqueue_full_post(_S, [], {error, queue_full}) -> true;
enqueue_full_post(_S, [], _) -> {error, enqueue_full_post}.

%%%% Case 5: Enqueuing when there is no available token
enqueue_no_tokens() -> enqueue().

enqueue_no_tokens_command(_S) ->
    {call, ?MODULE, enqueue_no_tokens, []}.

enqueue_no_tokens_pre(#state { tokens = T }) -> T == 0.

enqueue_no_tokens_next(S, _, _) -> S#state { queue_size = 1 }.

enqueue_no_tokens_post(_S, [], queueing) -> true;
enqueue_no_tokens_post(_S, [], Res) -> {error, {enqueue_no_tokens, Res}}.

%%%% Case 6: Enqueueing when there is a token and no worker
enqueue_to_work() -> enqueue().

enqueue_to_work_command(_S) ->
    {call, ?MODULE, enqueue_to_work, []}.

enqueue_to_work_pre(#state { tokens = 1, queue_size = 0, concurrency = 0 }) ->
    true;
enqueue_to_work_pre(_) -> false.

enqueue_to_work_next(S, _, _) ->
    S#state { tokens = 0, queue_size = 0, concurrency = 1 }.

enqueue_to_work_post(_S, [], {working, _}) -> true;
enqueue_to_work_post(_S, [], Res) -> {error, {enqueue_to_work, Res}}.

%%%% Case 7: Enqueueing when there is a worker    
enqueue_to_wait() -> enqueue().

enqueue_to_wait_command(_S) ->
    {call, ?MODULE, enqueue_to_wait, []}.

enqueue_to_wait_pre(#state { tokens = 1,
                             queue_size = 0,
                             concurrency = 1 }) -> true;
enqueue_to_wait_pre(_)                          -> false.

enqueue_to_wait_next(S, _, _) -> S#state { queue_size = 1 }.

enqueue_to_wait_post(_S, [], queueing) -> true;
enqueue_to_wait_post(_S, [], Res)      -> {error, {enqueue_to_wait, Res}}.

%% MARKING WORK AS DONE
%% ----------------------------------------------------------------------
done() ->
    {ok, Pid} = manager:mark_done(),
    eqc_helpers:fixpoint([whereis(manager) | manager:current_pids()]),
    manager:read_status(Pid).

%%%% Case 8: Done no more work
done_no_work() -> done().

done_no_work_command(_S) ->
    {call, ?MODULE, done_no_work, []}.

done_no_work_pre(#state { concurrency = 1, queue_size = 0 }) -> true;
done_no_work_pre(_) -> false.

done_no_work_next(S, _, _) -> S#state { concurrency = 0 }.

%%%% Case 9: Done, no more tokens
done_no_tokens() -> done().

done_no_tokens_command(_S) ->
    {call, ?MODULE, done_no_tokens, []}.

done_no_tokens_pre(#state { concurrency = 1, queue_size = 1, tokens = 0 }) ->
    true;
done_no_tokens_pre(_) -> false.

done_no_tokens_next(S, _, _) -> S#state { concurrency = 0 }.

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
weight(_S, enqueue_no_tokens) -> 80;
weight(_S, enqueue_full)      -> 100;
weight(_S, poll_full)         -> 100;
weight(_S, enqueue_to_work)   -> 100;
weight(_S, poll_empty_q)      -> 100;
weight(_S, done_no_work)      -> 100;
weight(_S, poll_to_work)      -> 150;
weight(_S, done_no_tokens)    -> 800;
weight(_S, enqueue_to_wait)   -> 800;
weight(_S, done_go_on)        -> 1500.

%% PROPERTIES
%% ----------------------------------------------------------------------

%% Check that the model can run
prop_model() ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
               begin
                   {ok, _Pid} = manager:start_link(),
                   application:start(safetyvalve),
                   {History, State, Result} = run_commands(?MODULE, Cmds),
                   application:stop(safetyvalve),
                   ok = manager:stop(),
                   timer:sleep(1000),
                   ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                       [History, State, Result]),
                             aggregate(command_names(Cmds), Result =:= ok))
               end)
           ).

t() ->
    application:start(syntax_tools),
    application:start(compiler),
    application:start(lager),
    eqc:module({numtests, 1000}, ?MODULE).
