-module(ws_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(InReq, State) ->
    case sv:run(ws_q, fun() ->
        {ok, Req2} = cowboy_req:reply(202, [], <<"Hello World">>, InReq),
        Req2
       end) of
        {ok, Req2} -> {ok, Req2, State};
        {error, queue_full} ->
          {ok, ReqFull} = cowboy_req:reply(503, [], <<"System Queue Full">>, InReq),
          {ok, ReqFull, State};
        {error, overload} ->
          {ok, ReqOL} = cowboy_req:reply(503, [], <<"System Overloaded">>, InReq),
          {ok, ReqOL, State}
    end.

terminate(_Req, _State) ->
    ok.
