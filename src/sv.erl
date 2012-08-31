-module(sv).

-export([run/2]).

%% @doc Enqueue a job on a queue
%% <p>Try to run `Fun' on queue `Name'. This means that either the
%% function will run straight away, or be queued for some time until
%% it is allowed to run (in case of an overload scenario). The
%% function will return either the result of `Fun' or an `{error,
%% Reason}' error term, describing the overload situation encountered.</p>
%% @end
run(Name, Fun) ->
    case sv_queue:ask(Name) of
        {go, Ref} ->
            Res = Fun(),
            sv_queue:done(Ref),
            Res;
        {error, Reason} ->
            {error, Reason}
    end.
