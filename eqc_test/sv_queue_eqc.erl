%% THE SIMPLEST POSSIBLE CASE
%%
%% When writing EQC test cases, begin by thinking in *microscopic*
%% test cases. That is, go for the smallest possible test case first
%% and then extend it. In our case, we have an extremely degenerate queue:
%%
%% * The concurrency level on the queue is 1.
%% * The queue size is 1, so there are at most a single waiter.
%%
%% The postconditions we want to check are:
%% * The concurrency level in the SUT is *never* more than 1.
%% * The queue size in the SUT is *never* more than 1.
%%
%% So if we spawn a new process when the queue is full, we expect that
%% new spawn to be denied queueing access since the queue is overloaded.
%%
%% We *do* want to generate random command sequences for our queue to
%% check this however, hence we write a quickcheck test case for it.
-module(sv_queue_eqc).

-compile([export_all]).

-include_lib("eqc/include/eqc_statem.hrl").
-eqc_group_commands(true).

-record(state,
        { concurrency,
          queue_size }).

%% The intial queue state
%% ----------------------------------------------------------------------
initial_state() ->
    #state { concurrency = 0,
             queue_size  = 0 }.

%% POLLING OF THE QUEUE
%% ----------------------------------------------------------------------
%% We may always poll the queue. This means we advance time on the
%% queue and ask it to add more tokens to its bucket regulator.
poll() ->
    sv_queue_test_1 ! poll.

poll_command(_S) ->
    {call, ?MODULE, poll, []}.

%% We state that you may always poll the queue and give it more
%% tokens. That is, in any possible state, time could pass without
%% anything happening.
poll_pre(_S) -> true.

%% SPAWNING A NEW PROCESS
%% ----------------------------------------------------------------------
%% We may spawn a new worker who will try to go the queue for work.
%% There is a couple of different cases, depending on the possible
%% states of the worker.

%% Case 1: The queue is empty and the worker will spawn to do work.
spawn_new_empty_q_command() ->
    {call, ?MODULE, spawn_new_process, []}.

%% We may only make a spawn on an empty queue, when the concurrency
%% level is 0 and the queue size is 0.
spawn_new_empty_q_pre(#state { concurrency = 0,
                               queue_size  = 0 }) -> true;
spawn_new_empty_q_pre(_) -> false.

%% When this call succeeds, you have a single worker doing work, and
%% you still have a queue size which is empty.
spawn_new_empty_q_next(#state { concurrency = 0,
                                queue_size  = 0 } = State, _V, []) ->
    State#state { concurrency = 1, queue_size = 0 }.

%% Case 2: The there is a worker, so we will queue
spawn_new_doing_work_command(_S) ->
    {call, ?MODULE, spawn_new_process, []}.

%% We can only queue when there is a worker doing work already.
spawn_new_doing_work_pre(#state { concurrency = 1, queue_size = 0}) -> true;
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

