%%% @doc This is a loose translation of the following link from ACM:
%%% http://queue.acm.org/appendices/codel.html
%%% http://pollere.net/CoDelnotes.html
%%% http://pollere.net/CoDel.html
%%%
%%% The document you want to read is
%%% "Controlling queue Delay" Kathleen Nichols, Van Jacobson, http://queue.acm.org/detail.cfm?id=2209336
%%% But also note that some of the other papers are interesting. Especially Kathie Nichols notes are of
%%% interest.
%%%
%%% @end
-module(sv_codel).

%% Public API
-export([new/0, in/3, out/2, len/1]).
-export([init/2, enqueue/3, dequeue/2]).

%% Scrutiny
-export([qstate/1]).

-type task() :: term().

-define(Q, sv_queue_ets).

%% Internal state
-record(state, {
    %% The underlying queue to use. For now, since we are mainly in a test phase, we just use a standard
    %% functional queue. But later the plan is to use a module here and then call the right kind of queue
    %% functions for that module.
    queue = ?Q:new(),
    
    %% The `dropping' field tracks if the CoDel system is in a dropping state or not.
    dropping = false,
    
    %% If we are dropping, this value tracks the point in time where the next packet should
    %% be dropped from the queue.
    drop_next = 0,
    
    %% First above time tracks when we first began seeing too much delay imposed by the queue.
    %% This value may be 0 in which case it means we have not seen such a delay.
    first_above_time = 0,
    
    %% This variable tracks how many packets/jobs were recently dropped from the queue.
    %% The value decays over time if no packets are dropped and is used to manipulate the control
    %% law of the queue.
    count = 0,
       
    %% The `interval' and `target' are configurable parameters, described in @see init/2.
    interval = 100, % ms
    target = 5 %ms
    }).

%% @doc Look at the queue state as a proplist
%% @end
-spec qstate(#state{}) -> [{atom(), term()}].
qstate(#state {
	queue = Q,
	dropping = Drop,
	drop_next = DN,
	interval = I,
	target = T,
	first_above_time = FAT,
	count = C
	}) ->
    [{queue, Q},
     {dropping, Drop},
     {drop_next, DN},
     {interval, I},
     {target, T},
     {first_above_time, FAT},
     {count, C}].

%% Queue API
%% -----------------------------
new() ->
  init(5*1000, 100*1000).
  
len(#state { queue = Q }) -> ?Q:len(Q).

in(Item, Ts, CoDelState) ->
    enqueue(Item, Ts, CoDelState).
    
out(Ts, CoDelState) ->
   dequeue(Ts, CoDelState).

%% @doc Initialize the CoDel state
%% <p>The value `Target' defines the delay target in ms. If the queue has a sojourn-time through the queue
%% which is above this value, then the queue begins to consider dropping packets.</p>
%% <p>The value `Interval' is the window we have to be above `Target' before we consider that there may be
%% problems. As such, it provides a hysteresis on the queue as well and small increases in latency does
%% not affect the queue.</p>
%% <p>Note that the interval makes sure we can use the queue as "good queue". If we get a sudden small
%% spike in jobs, then the queue will make sure they get smoothed out and processed with no loss of jobs.
%% But it also protects against "bad queue" where a standing queue won't dissipate due to consistent
%% overload of the system</p>
%% @end
-spec init(pos_integer(), pos_integer()) -> #state{}.
init(Target, Interval) when Target > Interval -> exit(misconfiguration);
init(Target, Interval) -> #state{ target = Target, interval = Interval }.

%% @doc Enqueue a packet
%% <p>Enqueue packet `Pkt' at time `TS' into the queue.</p>
%% @end
-spec enqueue(task(), term(), #state{}) -> #state{}.
enqueue(Pkt, TS, #state { queue = Q } = State) ->
  State#state { queue = ?Q:in({Pkt, TS}, TS, Q) }.

%% @doc Dequeue a packet from the CoDel system
%% Given a point in time, `Now' and a CoDel `State', extract the next task from it.
%% @end
-spec dequeue(Now, InState) ->
        {empty, [Pkt], OutState} | {drop, [Pkt], OutState} | {Pkt, [Pkt], OutState}
    when
      Now :: term(),
      Pkt :: task(),
      InState :: #state{},
      OutState :: #state{}.    
dequeue(Now, State) ->
  dequeue_(Now, dodequeue(Now, State)).

%% Internal functions
%% ---------------------------------------------------------

%% The control law defines the packet drop rate. Given a time T we drop the next packet at T+I, where
%% I is the interval. Now, if we need to drop yet another packet, we drop it at I/math:sqrt(C) where C
%% is the number of packets we have dropped so far in this round.
control_law(T, I, C) ->
  T + I / math:sqrt(C).

%% This is a helper function. It dequeues from the underlying queue and then analyzes the Sojourn
%% time together with the next function, dodequeue_.
dodequeue(Now, #state { queue = Q } = State) ->
  case ?Q:out(Now, Q) of
    {empty, [], NQ} ->
      sv:report(Now div 1000, {dodequeue, 0, 0}),
      {nodrop, empty, State#state { first_above_time = 0, queue = NQ }};
    {{Pkt, InT}, [], NQ} ->
      Sojourn = Now - InT,
      
      sv:report(Now div 1000, {dodequeue, ?Q:len(NQ), Sojourn div 1000}),
      dodequeue_(Now, Pkt, Sojourn, State#state { queue = NQ })
  end.

%% Case split:
%% The sojourn time through the queue is less than our target value. Thus, we should not drop, and
%% we reset when we were first above.
dodequeue_(_Now, Pkt, Sojourn, #state { target = T } = State) when Sojourn < T ->
    {nodrop, Pkt, State#state { first_above_time = 0 }};
%% We are above target, but this is the first time we are above target. We set up the point in time when
%% we went above the target to start tracking this.
dodequeue_(Now, Pkt, _Sojourn, #state { first_above_time = FAT, interval = I } = State) when FAT == 0 ->
    {nodrop, Pkt, State#state { first_above_time = Now + I }};
%% We have been above target for more than one interval. This is when we need to start dropping.
dodequeue_(Now, Pkt, _Sojourn, #state { first_above_time = FAT } = State) when Now >= FAT ->
    {drop, Pkt, State};
%% We are above target, but we have not yet been above target for a complete interval. Wait and see
%% what happens, but don't begin dropping packets just yet.
dodequeue_(_Now, Pkt, _Sojourn, State) ->
    {nodrop, Pkt, State}.


%% Dequeue worker. This drives the meat of the dequeue steps.
%% Case split:
%% We are in the dropping state, but are transitioning to not dropping.  
dequeue_(Now, {nodrop, Pkt, #state { dropping = true } = State}) ->
    dequeue_drop_next(Now, Pkt, State#state { dropping = false }, []);
%% We are in the dropping state and are to continue dropping.
dequeue_(Now, {drop, Pkt, #state { dropping = true } = State}) ->
    dequeue_drop_next(Now, Pkt, State, []);
%% We are not in the dropping state, but should start dropping.
dequeue_(Now, {drop, Pkt, #state { dropping = false } = State}) ->
    dequeue_start_drop(Now, Pkt, State);
%% Default case for normal operation.
dequeue_(_Now, {nodrop, Pkt, #state { dropping = false } = State}) ->
    {Pkt, [], State}.

%% Consider dropping the next packet from the queue. This function drives a loop until the next timepoint
%% where we should drop is in the future. The helper dequeue_drop_next_/3 carries out the book-keeping
dequeue_drop_next(Now, Pkt, #state { drop_next = DN, dropping = true } = State, Dropped)
        when Now >= DN ->
    dequeue_drop_next_(Now, dodequeue(Now, State), [Pkt | Dropped]);
dequeue_drop_next(_Now, Pkt, State, Dropped) ->
    {Pkt, Dropped, State}.

%% If the Sojourn time improves, we leave the dropping state.
dequeue_drop_next_(Now, {nodrop, Pkt, State}, Dropped) ->
    dequeue_drop_next(Now, Pkt, State#state { dropping = false }, Dropped);
%% We are still to drop packets, so update the count and the control law for the next loop round.
dequeue_drop_next_(
	Now,
	{drop, Pkt, #state { count = C, interval = I, drop_next = DN }  = State},
	Dropped) ->
    dequeue_drop_next(
    	Now,
    	Pkt,
    	State#state { count = C + 1, drop_next = control_law(DN, I, C + 1) },
    	Dropped).

%% Function for setting up the dropping state. When we start dropping, we evaluate a bit on
%% how long ago we last dropped. If we did this recently, we do not start off from the bottom of
%% the control law, but rather pick a point a bit up the function. On the other hand, if it is a long time
%% ago, we just pick the usual starting point of 1.
dequeue_start_drop(Now, Pkt, #state { drop_next = DN, interval = Interval, count = Count } = State)
	when Now - DN < Interval, Count > 2 ->
    {drop, [Pkt], State#state {
    	dropping = true,
    	count = Count - 2,
    	drop_next = control_law(Now, Interval, Count - 2) }};
dequeue_start_drop(Now, Pkt, #state { interval = I } = State) ->
    {drop, [Pkt], State#state {
    	dropping = true,
    	count = 1,
    	drop_next = control_law(Now, I, 1) }}.
