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
%% {1, 0, 0} -> done -> {0, 0, 0}
%% {1, 0, 1} -> done -> {0, 0, 1}

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

%% The intial queue state
%% ----------------------------------------------------------------------
initial_state() ->
    #state { concurrency = 0,
             queue_size  = 0,
             tokens      = 1 }. %% Initialized to the rate of the queue

%% POLLING OF THE QUEUE
%% ----------------------------------------------------------------------

%%%% Case 1: polling the queue, when the token bucket is full
poll_full() ->
    todo.

poll_full_command(_S) ->
    {call, ?MODULE, poll_full, []}.

%% This case matches, when the token bucket is full
poll_full_pre(#state { tokens = T }) -> T == 1.

poll_full_next(S) -> S.

%%%% Case 2: polling the queue, when there is no-one queued
poll_empty_q() ->
    todo.

pull_empty_q_command(_S) ->
    {call, ?MODULE, poll_empty_q, []}.

%% This case matches if we are lacking a token and the queue is empty
poll_empty_q_pre(#state { tokens = T, queue_size = QS }) ->
    T == 0 andalso QS == 0.

poll_empty_q_next(S) -> S#state { tokens = 1 }.

%%%% Case 3: polling the queue, when there is a waiter and no-one working
poll_to_work() ->
    todo.

poll_to_work_command(_S) ->
    {call, ?MODULE, poll_to_work, []}.

poll_to_work_pre(#state { tokens = T, queue_size = QS, concurrency = C }) ->
    T == 0 andalso QS == 1 andalso C == 0.

poll_to_work_next(S) ->
    S#state { concurrency = 1, queue_size = 0, tokens = 0 }.
   
%% SPAWNING A NEW PROCESS
%% ----------------------------------------------------------------------
%% We may spawn a new worker who will try to go the queue for work.
%% There is a couple of different cases, depending on the possible
%% states of the worker.

%% Case 1: The queue is empty and the worker will spawn to do work.
spawn_new_empty_q_command() ->
    {call, ?MODULE, spawn_new_process, []}.

%% We may only make a spawn on an empty queue, when the concurrency
%% level is 0 and the queue size is 0. Furthermore, there should be a
%% token we can consume.
spawn_new_empty_q_pre(#state { concurrency = 0,
                               queue_size  = 0,
                               tokens      = 1 }) -> true;
spawn_new_empty_q_pre(_) -> false.

%% When this call succeeds, you have a single worker doing work, and
%% you still have a queue size which is empty.
spawn_new_empty_q_next(#state { concurrency = 0,
                                queue_size  = 0 } = State, _V, []) ->
    State#state { concurrency = 1, queue_size = 0, tokens = 0 }.

%% Case 2: The there is a worker, so we will queue
spawn_new_doing_work_command(_S) ->
    {call, ?MODULE, spawn_new_process, []}.

%% We can only queue when there is a worker doing work already.
spawn_new_doing_work_pre(#state { concurrency = 1,
                                  queue_size = 0,
                                  tokens = 1}) -> true;
spawn_new_doing_work_pre(_) -> false.

spawn_new_doing_work_next(#state { queue_size = 0 } = State, _V, []) ->
    State#state { queue_size = 1 }.

%% Case 3: A new worker is denied access since the queue is full
spawn_new_queue_full_command(_S) ->
    {call, ?MODULE, spawn_new_process, []}.

%% This will only happen when we are doing work and the queue is
%% already full.
spawn_new_queue_full_pre(#state { concurrency = 1, queue_size = 1 }) -> true;
spawn_new_queue_full_pre(_) -> false.

%% TODO: Postcondition check on the denial here.

%% MARKING WORK AS DONE
%% ----------------------------------------------------------------------
mark_done() ->
    todo.

%% Again, when work is being marked as done by a process, we want to
%% be able to split on different cases in the model, depending on what
%% is happening.

%% Case 1: The queue is now empty and we are done with the last amount
%% of work.
mark_done_empty_q_command(_S) ->
    {call, ?MODULE, mark_done, []}.

mark_done_empty_q_pre(#state { concurrency = 1, queue_size = 0}) -> true;
mark_done_empty_q_pre(_) -> false.

mark_done_empty_q_next(#state { concurrency = 1, queue_size = 0 } = State,
                       _V,
                       []) ->
    State#state { concurrency = 0 }.

%% Case 2: Done, but there is a waiter in the queue. This means that
%% the waiter is now the process doing work on the queue.
mark_done_waiter_command(_S) ->
    {call, ?MODULE, mark_done, []}.

mark_done_waiter_pre(#state { concurrency = 1, queue_size = 1}) -> true;
mark_done_waiter_pre(_) -> false.

mark_done_waiter_next(#state { concurrency = 1, queue_size = 1 } = State,
                      _V,
                      []) ->
    State#state { concurrency = 1,
                  queue_size = 0 }.

