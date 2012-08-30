-module(sv).

-export([run/2]).

run(Name, Fun) ->
    case sv_queue:ask(Name) of
        {go, Ref} ->
            Fun(),
            sv_queue:done(Ref);
        {error, Reason} ->
            {error, Reason}
    end.
