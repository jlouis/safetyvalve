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
-export([start_link/0]).
-export([spawn_worker/0,
         doing_work/0,
         status/1,
         read_status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          workers
         }).

%%%===================================================================

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

%%%===================================================================

%% @private
init([]) ->
    {ok, #state{ workers = [] }}.

%% @private
handle_call(spawn_worker, _From, #state { workers = Workers } = State) ->
    {ok, Pid} = worker:start_link(),
    {reply, ok, State#state {workers = [{Pid, queueing} | Workers ] }};
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
    Response = lists:keyfind(Pid, 1, State#state.workers),
    {reply, Response, State};
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
