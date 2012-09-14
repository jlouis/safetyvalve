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

-export([poll/1, ask/1, done/2, q/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(conf, { hz, rate, token_limit, size, concurrency }).
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
    #conf { hz          = proplists:get_value(hz, Conf),
            rate        = proplists:get_value(rate, Conf),
            token_limit = proplists:get_value(token_limit, Conf),
            size        = proplists:get_value(size, Conf),
            concurrency = proplists:get_value(concurrency, Conf) }.

ask(Name) ->
    gen_server:call(Name, ask, infinity).

done(Name, Ref) ->
    gen_server:call(Name, {done, Ref}, infinity).

poll(Name) ->
    Name ! poll.

q(Name, Atom) ->
    gen_server:call(Name, {q, Atom}).

%%%===================================================================

%% @private
init([Conf]) ->
    case Conf#conf.hz of
        undefined -> ok;
        K when is_integer(K) ->
            repoll(Conf)
    end,
    {ok, #state{ conf = Conf,
                 queue = queue:new(),
                 tokens = Conf#conf.rate,
                 tasks = gb_sets:empty() }}.

%% @private
handle_call({q, tokens}, _, #state { tokens = K } = State) ->
    {reply, K, State};
handle_call(ask, {Pid, _Tag} = From, #state { tokens = K,
                                       conf = Conf,
                                       queue = Q,
                                       tasks = Tasks } = State) when K > 0 ->
    %% Let the guy run, since we have excess tokens:
    case analyze_tasks(Tasks, Conf) of
        concurrency_full ->
            case enqueue(From, Q, Conf) of
                {ok, NQ} ->
                    {noreply, State#state { queue = NQ}};
                queue_full ->
                    {reply, {error, queue_full}, State}
            end;
        go ->
            Ref = erlang:monitor(process, Pid),
            {reply, {go, Ref}, State#state { tokens = K-1,
                                             tasks  = gb_sets:add_element(Ref, Tasks) }}
    end;
handle_call(ask, From, #state { tokens = 0,
                                conf = Conf,
                                queue = Q } = State) ->
    %% No more tokens, queue the guy
    case enqueue(From, Q, Conf) of
        {ok, NQ} ->
            {noreply, State#state { queue = NQ } };
        queue_full ->
            {reply, {error, queue_full}, State}
    end;
handle_call({done, Ref}, _From, #state { tasks = Tasks } = State) ->
    true = erlang:demonitor(Ref),
    NewState = State#state { tasks = gb_sets:del_element(Ref, Tasks) },
    {reply, ok, process_queue(NewState) };
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', Ref, _, _, _}, #state { tasks = TS } = State) ->
    {noreply, State#state { tasks = gb_sets:del_element(Ref, TS) }};
handle_info(poll, #state { conf = C } = State) ->
    lager:debug("Poll invoked"),
    NewState = process_queue(refill_tokens(State)),
    repoll(C),
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
process_queue(#state { queue = Q, tokens = K, tasks = Ts } = State) ->
    {NK, NQ, NTs} = process_queue(K, Q, Ts),
    State#state { queue = NQ, tokens = NK, tasks = NTs }.

process_queue(0, Q, TS) -> {0, Q, TS};
process_queue(K, Q, TS) ->
    case queue:out(Q) of
        {{value, {Pid, _} = From}, Q2} ->
            Ref = erlang:monitor(process, Pid),
            gen_server:reply(From, {go, Ref}),
            process_queue(K-1, Q2, gb_sets:add_element(Ref, TS));
        {empty, Q2} ->
            {K, Q2, TS}
    end.

enqueue(Term, Q, #conf { size = Sz } ) ->
    case queue:len(Q) of
        K when K < Sz ->
            {ok, queue:in(Term, Q)};
        K when K == Sz ->
            queue_full
    end.

%% @doc Analyze tasks to see if we are close to the limit
analyze_tasks(Tasks, #conf { concurrency = Limit }) ->
    case gb_sets:size(Tasks) of
        K when K < Limit ->
            go;
        K when K == Limit ->
            concurrency_full
    end.

%% @doc Refill the tokens in the bucket
%% @end
refill_tokens(#state { tokens = K,
                       conf = #conf { rate = Rate,
                                      token_limit = TL }} = State) ->
    TokenCount = min(K + Rate, TL),
    State#state { tokens = TokenCount }.

repoll(#conf { hz = undefined }) -> ok;
repoll(#conf { hz = Hz }) ->
    erlang:send_after(Hz, self(), poll).
