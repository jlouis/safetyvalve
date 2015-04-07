%%%-------------------------------------------------------------------
%%% @author Jesper Louis Andersen <>
%%% @copyright (C) 2012, Jesper Louis Andersen
%%% @doc
%%% Queue Protocol:
%%%
%%% The queue is implemented as a gen_server with a simple ask/done
%%% protocol. A worker can send the server an `ask' message which means you
%%% ask to get a token for doing work. This will block the worker
%%% until either:
%%%   * Timeout. You were queued for too long as per queue configuration.
%%%   * Queue full. The queue already has too many workers in it
%%%   * Opaque Ref - the worker is now monitored and ready to do work.
%%%
%%% Once granted the opaque reference, the worker can start doing work.
%%% When the worker is done, the protocol mandates that you either
%%% send the message `{done, Ref}' where `Ref' is the opaque
%%% reference, or that you exit the process. In the latter case the
%%% monitor from the queue process ensures that we can continue.
%%% @end
%%% Created : 30 Aug 2012 by Jesper Louis Andersen <>
%%%-------------------------------------------------------------------
-module(sv_queue).

-behaviour(gen_server).

%% API
-export([start_link/2]).

-export([parse_configuration/1]).

-export([replenish/1, ask/1, ask/2, done/3, q/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(conf, { hz, rate, token_limit, size, concurrency, queue_type, queue_args }).
-type conf() :: #conf{}.

-record(state, {
          %% Conf is the configuration object this queue is configured
          %% with. It is a place to query about conf options
          conf,

          %% Queue reference to the queue we have of workers that are
          %% waiting to be allowed to execute. Also maintains the
          %% current queue size.
          queue,

          %% Tokens is a counter of how many tokens that are in the
          %% Token Bucket Regulator right now.
          tokens,

          %% Tasks is a set which contains the monitor references on
          %% the currently executing tasks. It is used to make sure
          %% that we maintain the concurrency limit correctly if tasks
          %% crash. Also keeps track of the current task concurrency value
          tasks,
          task_count = 0 }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the server, can either be registered or not
%% @end
start_link(undefined, Conf) ->
    gen_server:start_link(?MODULE, Conf, []);
start_link(Name, Conf) ->
    gen_server:start_link({local, Name}, ?MODULE, Conf, []).

-spec parse_configuration(proplists:proplist()) -> conf().
parse_configuration(Conf) ->
    {QueueType, QueueArgs} =
        case proplists:get_value(queue_type, Conf, sv_queue_ets) of
            {QT, Args} -> {QT, Args};
            QT when is_atom(QT) -> {QT, []}
        end,
    #conf {
      hz = proplists:get_value(hz, Conf),
      rate = proplists:get_value(rate, Conf),
      token_limit = proplists:get_value(token_limit, Conf),
      size = proplists:get_value(size, Conf),
      concurrency = proplists:get_value(concurrency, Conf),
      queue_type = QueueType,
      queue_args = QueueArgs
    }.

ask(Name) ->
	ask(Name, sv:timestamp()).

ask(Name, Timestamp) ->
    gen_server:call(Name, {ask, Timestamp}, infinity).

done(Name, Ref, Timestamp) ->
    gen_server:call(Name, {done, Timestamp, Ref}, infinity).

replenish(Name) ->
    Name ! replenish,
    ok.

q(Name, Atom) ->
    gen_server:call(Name, {q, Atom}).

%%%===================================================================

%% @private
init(Conf) ->
    set_timer(Conf),
    QT = Conf#conf.queue_type,
    QArgs = Conf#conf.queue_args,
    TID = ets:new(tasks, [protected, set]),
    {ok, #state{ conf = Conf,
                 queue = apply(QT, new, QArgs),
                 tokens = Conf#conf.token_limit,
                 tasks = TID }}.

%% @private
handle_call({q, tokens}, _, #state { tokens = K } = State) ->
    {reply, K, State};
handle_call({q, configuration}, _, #state { conf = Conf } = State) ->
    {reply, Conf, State};
handle_call({ask, Timestamp}, {Pid, _Tag} = From,
		 #state {
		 	tokens = K,
			conf = Conf,
			queue = Q,
			tasks = TID,
			task_count = TC } = State) when K > 0 ->
    %% Let the guy run, since we have excess tokens:
    Ref = erlang:monitor(process, Pid),
    case analyze_tasks(TC, Conf) of
        concurrency_full ->
            case enqueue({From, Ref}, Timestamp, Q, Conf) of
                {ok, NQ} ->
                    true = ets:insert_new(TID, {Ref, {queueing, {From, Ref}, Timestamp}}),
                    {noreply, State#state { queue = NQ}};
                queue_full ->
                    {reply, {error, queue_full}, State}
            end;
        {go, _K} ->
            true = ets:insert_new(TID, {Ref, working}),
            {reply, {go, Ref}, State#state { tokens = K-1, task_count = TC+1}}
    end;
handle_call({ask, Timestamp}, {Pid, _Tag} = From,
	#state {
		tokens = 0,
		conf = Conf,
		queue = Q,
		tasks = TID } = State) ->
    %% No more tokens, queue the guy
    Ref = erlang:monitor(process, Pid),
    case enqueue({From, Ref}, Timestamp, Q, Conf) of
        {ok, NQ} ->
            true = ets:insert_new(TID, {Ref, {queueing, {From, Ref}, Timestamp}}),
            {noreply, State#state { queue = NQ } };
        queue_full ->
            {reply, {error, queue_full}, State}
    end;
handle_call({done, Now, Ref}, _From, #state { tasks = TID, task_count = TC } = State) ->
    true = erlang:demonitor(Ref),
    true = ets:delete(TID, Ref),
    {reply, ok, process_queue(Now, State#state { task_count = TC - 1}) };
handle_call(Request, _From, State) ->
    lager:error("Unknown call request: ~p", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', Ref, _, _, _}, #state {
	tasks = TID,
	task_count = TC,
	queue = Q,
	conf = #conf { queue_type = QT } } = State) ->
    Now = sv:timestamp(),
    NewState = case ets:lookup_element(TID, Ref, 2) of
        working ->
            true = ets:delete(TID, Ref),
            State#state { task_count = TC - 1};
        {queueing, Item, TS} ->
            true = ets:delete(TID, Ref),
            NQ = QT:remove(Item, TS, Q),
            State#state { queue = NQ }
    end,
    {noreply, process_queue(Now, NewState)};
handle_info({replenish, TS}, State) ->
    NewState = process_queue(TS, refill_tokens(State)),
    {noreply, NewState};
handle_info(replenish, #state { conf = C } = State) ->
    Now = sv:timestamp(),
    NewState = process_queue(Now, refill_tokens(State)),
    set_timer(C),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================

%% @doc Try to use up tokens for queued items
%% @end
process_queue(Now, #state { queue = Q, tokens = K, tasks = TID, conf = Conf, task_count = TC } = State) ->
    case analyze_tasks(TC, Conf) of
        concurrency_full ->
            State;
        {go, RemainingConc} ->
            ToStart = min(RemainingConc, K),
            QT = Conf#conf.queue_type,
            {Started, NQ} = process_queue(Now, ToStart, Q, QT, TID),
            State#state { queue = NQ, tokens = K - Started, task_count = TC + Started}
    end.

process_queue(Now, Rem, Q, QT, TID) ->
    process_queue(Now, Rem, Q, QT, TID, 0).

process_queue(_Now, 0, Q, _QT, _TID, Started) ->
    {Started, Q};
process_queue(Now, K, Q, QT, TID, Started) ->
    case QT:out(Now, Q) of
        {drop, Dropped, Q2} ->
            drop(Dropped),
            process_queue(Now, K, Q2, QT, TID, Started);
        { {From, Ref}, Dropped, Q2} ->
            drop(Dropped),
            gen_server:reply(From, {go, Ref}),
            true = ets:insert(TID, {Ref, working}),
            process_queue(Now, K-1, Q2, QT, TID, Started+1);
        {empty, Dropped, Q2} ->
            drop(Dropped),
            {Started, Q2}
    end.

enqueue(Term, Timestamp, Q, #conf { size = Sz, queue_type = QT } ) ->
    case QT:len(Q) of
        K when K < Sz ->
            {ok, QT:in(Term, Timestamp, Q)};
        K when K == Sz ->
            queue_full
    end.

%% @doc Analyze tasks to see if we are close to the limit
analyze_tasks(TC, #conf { concurrency = Limit }) when TC < Limit -> {go, Limit - TC};
analyze_tasks(TC, #conf { concurrency = Limit }) when TC == Limit -> concurrency_full.

%% @doc Refill the tokens in the bucket
%% @end
refill_tokens(#state { tokens = K,
                       conf = #conf { rate = Rate,
                                      token_limit = TL }} = State) when K >= 0 ->
    TokenCount = min(K + Rate, TL),
    State#state { tokens = TokenCount }.

set_timer(#conf { hz = undefined }) -> ok;
set_timer(#conf { hz = Hz }) ->
    erlang:send_after(Hz, self(), replenish).

drop(Tasks) ->
    [begin
        erlang:demonitor(Ref, [flush]),
        gen_server:reply(From, {error, overload})
     end || {From, Ref} <- Tasks],
    ok.
