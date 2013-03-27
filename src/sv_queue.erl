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
          tasks }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the server
%% @end
start_link(Name, Conf) ->
    gen_server:start_link({local, Name}, ?MODULE, [Conf], []).

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
    Name ! replenish.

q(Name, Atom) ->
    gen_server:call(Name, {q, Atom}).

%%%===================================================================

%% @private
init([Conf]) ->
    set_timer(Conf),
    QT = Conf#conf.queue_type,
    QArgs = Conf#conf.queue_args,
    {ok, #state{ conf = Conf,
                 queue = apply(QT, new, QArgs),
                 tokens = min(Conf#conf.rate, Conf#conf.token_limit),
                 tasks = gb_sets:empty() }}.

%% @private
handle_call({q, tokens}, _, #state { tokens = K } = State) ->
    {reply, K, State};
handle_call({q, len}, _, #state { queue = Q, conf = C} = State) ->
    QT = C#conf.queue_type,
    {reply, QT:len(Q), State};
handle_call({ask, Timestamp}, {Pid, _Tag} = From,
		 #state {
		 	tokens = K,
			conf = Conf,
			queue = Q,
			tasks = Tasks } = State) when K > 0 ->
    sv:report(Timestamp, ask),
    %% Let the guy run, since we have excess tokens:
    case analyze_tasks(Tasks, Conf) of
        concurrency_full ->
            case enqueue(From, Timestamp, Q, Conf) of
                {ok, NQ} ->
                    {noreply, State#state { queue = NQ}};
                queue_full ->
                    {reply, {error, queue_full}, State}
            end;
        {go, _K} ->
            Ref = erlang:monitor(process, Pid),
            {reply, {go, Ref}, State#state { tokens = K-1,
                                             tasks  = gb_sets:add_element(Ref, Tasks) }}
    end;
handle_call({ask, Timestamp}, From,
	#state {
		tokens = 0,
		conf = Conf,
		queue = Q } = State) ->
    sv:report(Timestamp, ask),
    %% No more tokens, queue the guy
    case enqueue(From, Timestamp, Q, Conf) of
        {ok, NQ} ->
            {noreply, State#state { queue = NQ } };
        queue_full ->
            {reply, {error, queue_full}, State}
    end;
handle_call({done, Now, Ref}, _From, #state { tasks = Tasks } = State) ->
    sv:report(Now, {done, Ref}),
    true = erlang:demonitor(Ref, [flush]),
    NewState = State#state { tasks = gb_sets:del_element(Ref, Tasks) },
    {reply, ok, process_queue(Now, NewState) };
handle_call(Request, _From, State) ->
    lager:error("Unknown call request: ~p", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', Ref, _, _, _}, #state { queue = Q, tasks = TS, conf = Conf } = State) ->
    Now = sv:timestamp(),
    sv:report(Now, 'DOWN'),
    QT = Conf#conf.queue_type,
    PrunedTS = gb_sets:del_element(Ref, TS),
    case TS == PrunedTS of
      false -> {noreply, process_queue(Now, State#state { tasks = PrunedTS }) };
      true -> {noreply, State#state { queue = QT:prune(Ref, Q) }}
    end;
handle_info({replenish, TS}, State) ->
    sv:report(TS, replenish),
    NewState = process_queue(TS, refill_tokens(State)),
    {noreply, NewState};
handle_info(replenish, #state { conf = C } = State) ->
    Now = sv:timestamp(),
    sv:report(Now, replenish),
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
process_queue(Now, #state { queue = Q, tokens = K, tasks = Ts, conf = Conf } = State) ->
    case analyze_tasks(Ts, Conf) of
        concurrency_full ->
            State;
        {go, RemainingConc} ->
            ToStart = min(RemainingConc, K),
            QT = Conf#conf.queue_type,
            {Started, NQ, NTs} = process_queue(Now, ToStart, Q, QT, Ts),
            State#state { queue = NQ, tokens = K - Started, tasks = NTs }
    end.

process_queue(Now, Rem, Q, QT, TS) ->
    process_queue(Now, Rem, Q, QT, TS, 0).

process_queue(_Now, 0, Q, _QT, TS, Started) ->
    {Started, Q, TS};
process_queue(Now, K, Q, QT, TS, Started) ->
    case QT:out(Now, Q) of
        {drop, Dropped, Q2} ->
            drop(Dropped),
            process_queue(Now, K, Q2, QT, TS, Started);
        {{From, Ref}, Dropped, Q2} ->
            drop(Dropped),
            sv:report(Now, {go, Ref}),
            gen_server:reply(From, {go, Ref}),
            process_queue(Now, K-1, Q2, QT, gb_sets:add_element(Ref, TS), Started+1);
        {empty, Dropped, Q2} ->
            drop(Dropped),
            {Started, Q2, TS}
    end.

enqueue({Pid, _Tag} = From, Timestamp, Q, #conf { size = Sz, queue_type = QT } ) ->
    case QT:len(Q) of
        K when K < Sz ->
            Ref = erlang:monitor(process, Pid),
            {ok, QT:in({From, Ref}, Timestamp, Q)};
        K when K == Sz ->
            queue_full
    end.

%% @doc Analyze tasks to see if we are close to the limit
analyze_tasks(Tasks, #conf { concurrency = Limit }) ->
    case gb_sets:size(Tasks) of
        K when K < Limit ->
            {go, Limit - K};
        K when K == Limit ->
            concurrency_full
    end.

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
    [gen_server:reply(F, {error, overload}) || F <- Tasks],
    ok.
