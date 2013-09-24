-module(sv).

-export([timestamp/0, ask/2, done/3]).
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

%% @doc ask/2 requests the use of a resource in safetyvalve
%% <p>Ask for the use of a `Queue' at timepoint `T'. Returns either `{go, Ref}' if
%% you are allowed to use the resource or `{error, Reason}' in case of an error</p>
%% <p>The timepoint `T' should be generated via a call to `sv:timestamp()'. Also, note
%% that this call will block until the resource is either given, or the system gives
%% up on processing the request because it has exceeded some queueing threshold.</p>
%% <p>When you are done processing, you are obliged to call `sv:done(Queue, Ref, TE)'
%% where `Ref' is the given reference and `TE' is a time endpoint as given by
%% a call to `sv:timestamp()'.
%% @end
-spec ask(Queue, T) -> {go, Ref} | {error, Reason}
  when
    Queue :: atom(),
    T :: integer(),
    Ref :: term(), % Opaque
    Reason :: term().
ask(QN, T) ->
  sv_queue:ask(QN, T).
  
%% @doc done/3 relinquishes a resource yet again to the queue
%% <p>Call this function when you are done with using a resource. @see ask/2 for the
%% documentation of how to invoke this function.</p>
-spec done(Queue, Ref, TE) -> ok
  when
    Queue :: atom(),
    Ref :: term(),
    TE :: integer().
done(QN, R, TE) ->
  sv_queue:done(QN, R, TE).

%% @private
report(_T, _Event) ->
    hopefully_traced.

%% @doc Construct a timestamp in a canonical way for Safetyvalve.
-spec timestamp() -> integer().
timestamp() ->
	%% Timestamps *have* to be unique. Calling erlang:now/0 makes sure
	%% this happens. But you can use any ordered term if you want, for instance
	%% {os:timestamp(), self()} or {os:timestamp(), ref()}.
	{Mega, Secs, Micro} = erlang:now(),
	(Mega * 1000000 + Secs) * 1000000 + Micro.

