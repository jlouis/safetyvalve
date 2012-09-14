%%%-------------------------------------------------------------------
%%% @author Jesper Louis Andersen <>
%%% @copyright (C) 2012, Jesper Louis Andersen
%%% @doc
%%%
%%% @end
%%% Created :  9 Sep 2012 by Jesper Louis Andersen <>
%%%-------------------------------------------------------------------
-module(manager).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0]).
-export([spawn_worker/0,
         current_pids/0,
         doing_work/0,
         mark_done/0,
         status/1,
         read_status/1,
         stop/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          workers
         }).

%%%===================================================================

%% @doc
%% Start the server, but do not link it
%% @end
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% Starts the server
%% @end
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

spawn_worker() ->
    gen_server:call(?MODULE, spawn_worker).

doing_work() ->
    gen_server:call(?MODULE, doing_work, infinity).

status(Term) ->
    gen_server:call(?MODULE, {status, Term}).

read_status(Pid) ->
    gen_server:call(?MODULE, {read_status, Pid}).

mark_done() ->
    gen_server:call(?MODULE, mark_done).

current_pids() ->
    gen_server:call(?MODULE, current_pids).

stop() ->
    gen_server:call(?MODULE, stop).

%%%===================================================================

%% @private
init([]) ->
    {ok, #state{ workers = [] }}.

%% @private
handle_call(spawn_worker, _From, #state { workers = Workers } = State) ->
    Pid = worker:start(),
    {reply, {ok, Pid}, State#state {workers = [{Pid, queueing} | Workers ] }};
handle_call(doing_work, {Pid, _Tag} = From,
            #state { workers = Workers } = State) ->
    {noreply, State#state {
                workers =
                    lists:keyreplace(Pid, 1, Workers, {Pid, {working, From}})
               }};
handle_call({status, S}, {Pid, _Tag}, #state { workers = Workers } = State) ->
    {reply,
     ok,
     State#state {
       workers =
           lists:keyreplace(Pid, 1, Workers, {Pid, {res, S}}) }};
handle_call({read_status, Pid}, _From, State) ->
    {_Key, Response} = lists:keyfind(Pid, 1, State#state.workers),
    {reply, Response, State};
handle_call(current_pids, _From, #state { workers = Workers } = State) ->
    Pids = [P || {P, _} <- Workers],
    {reply, Pids, State};
handle_call(mark_done, _From, #state { workers = Workers } = State) ->
    case [{Pid, From} || {Pid, {working, From}} <- Workers] of
        [] ->
            {reply, {error, none_working}, State};
        [{Pid, From} | _T] ->
            gen_server:reply(From, done),
            {reply, {ok, Pid}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
