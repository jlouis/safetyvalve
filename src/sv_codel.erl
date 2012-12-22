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

-export([init/2, enqueue/3, dequeue/2]).

%% Scrutiny
-export([qstate/1]).

-record(state,
    { queue = queue:new(),
      dropping = false,
      drop_next = 0,
      interval = 100, % ms
      target = 5, %ms
      first_above_time = 0,
      count = 0
    }).

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

init(Target, Interval) -> #state{ target = Target, interval = Interval }.

enqueue(Pkt, TS, #state { queue = Q } = State) ->
  State#state { queue = queue:in({Pkt, TS}, Q) }.


control_law(T, I, C) ->
  T + I / math:sqrt(C).

dodequeue(Now, #state { queue = Q } = State) ->
  case queue:out(Q) of
    {empty, NQ} ->
      {nodrop, empty, State#state { first_above_time = 0, queue = NQ }};
    {{value, {Pkt, InT}}, NQ} ->
      Sojourn = Now - InT,
      
      dodequeue_(Now, Pkt, Sojourn, State#state { queue = NQ })
  end.
  
dodequeue_(_Now, Pkt, Sojourn, #state { target = T } = State) when Sojourn < T ->
    {nodrop, Pkt, State#state { first_above_time = 0 }};
dodequeue_(Now, Pkt, _Sojourn, #state { first_above_time = FAT, interval = I } = State) when FAT == 0 ->
    {nodrop, Pkt, State#state { first_above_time = Now + I }};
dodequeue_(Now, Pkt, _Sojourn, #state { first_above_time = FAT } = State) when Now >= FAT ->
    {drop, Pkt, State};
dodequeue_(_Now, Pkt, _Sojourn, State) ->
    {nodrop, Pkt, State}.

dequeue(Now, State) ->
  dequeue_(Now, dodequeue(Now, State)).
  
dequeue_(Now, {nodrop, Pkt, #state { dropping = true } = State}) ->
    dequeue_drop_next(Now, Pkt, State#state { dropping = false }, []);
dequeue_(Now, {drop, Pkt, #state { dropping = true } = State}) ->
    dequeue_drop_next(Now, Pkt, State, []);
dequeue_(Now, {drop, Pkt, #state { dropping = false } = State}) ->
    dequeue_start_drop(Now, Pkt, State);
dequeue_(_Now, {nodrop, Pkt, #state { dropping = false } = State}) ->
    {Pkt, [], State}.

dequeue_drop_next(Now, Pkt, #state { drop_next = DN, dropping = true } = State, Dropped)
        when Now >= DN ->
    dequeue_drop_next_(Now, dodequeue(Now, State), [Pkt | Dropped]);
dequeue_drop_next(_Now, Pkt, State, Dropped) ->
    {Pkt, Dropped, State}.

dequeue_drop_next_(Now, {nodrop, Pkt, State}, Dropped) ->
    dequeue_drop_next(Now, Pkt, State#state { dropping = false }, Dropped);
dequeue_drop_next_(
	Now,
	{drop, Pkt, #state { count = C, interval = I, drop_next = DN }  = State},
	Dropped) ->
    dequeue_drop_next(
    	Now,
    	Pkt,
    	State#state { count = C + 1, drop_next = control_law(DN, I, C + 1) },
    	Dropped).

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
