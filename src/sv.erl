-module(sv).

-export([timestamp/0]).
-export([run/2, run/3]).

%% @doc Enqueue a job on a queue
%% <p>Try to run `Fun' on queue `Name'. The `Fun' is run at time `TP'.
%% This means that either the
%% function will run straight away, or be queued for some time until
%% it is allowed to run (in case of an overload scenario). The
%% function will return either the result of `Fun' or an `{error,
%% Reason}' error term, describing the overload situation encountered.</p>
%% @end
run(Name, TimePoint, Fun) ->
    case sv_queue:ask(Name, TimePoint) of
        {go, Ref} ->
            Res = Fun(),
            sv_queue:done(Name, Ref),
            Res;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc A variant where the time is injected automatically
%% @end
run(Name, Fun) ->
	run(Name, timestamp(), Fun).

%% @doc Construct a timestamp in a canonical way for Safetyvalve.
timestamp() ->
	%% Timestamps *have* to be unique. Calling erlang:now/0 makes sure
	%% this happens. But you can use any ordered term if you want, for instance
	%% {os:timestamp(), self()} or {os:timestamp(), ref()}.
	erlang:now().

