-module(sv).

-export([timestamp/0]).
-export([run/2]).
%% Internal API
-export([report/2]).

%% @doc Enqueue a job on a queue
%% <p>Try to run `Fun' on queue `Name'. The `Fun' is run at time `TP'.
%% This means that either the
%% function will run straight away, or be queued for some time until
%% it is allowed to run (in case of an overload scenario). The
%% function will return either the result of `Fun' or an `{error,
%% Reason}' error term, describing the overload situation encountered.</p>
%% @end

-spec run(Name, Fun) -> {ok, Result} | {error, Reason}
    when
      Name :: atom(),
      Fun :: fun (() -> term),
      Result :: term(),
      Reason :: term().
run(Name, Fun) ->
    StartPoint = timestamp(),
    case sv_queue:ask(Name, StartPoint) of
        {go, Ref} ->
            Res = Fun(),
            EndPoint = timestamp(), 
            sv_queue:done(Name, Ref, EndPoint),
            {ok, Res};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
report(_T, _Event) ->
    hopefully_traced.

%% @doc Construct a timestamp in a canonical way for Safetyvalve.
-spec timestamp() -> term().
timestamp() ->
	%% Timestamps *have* to be unique. Calling erlang:now/0 makes sure
	%% this happens. But you can use any ordered term if you want, for instance
	%% {os:timestamp(), self()} or {os:timestamp(), ref()}.
	{Mega, Secs, Micro} = erlang:now(),
	(Mega * 1000000 + Secs) * 1000000 + Micro.

