-module(eqc_helpers).

-export([fixpoint/1]).


fixpoint(Pids) ->
    fixpoint(Pids, test(Pids)).

fixpoint(Pids, State) ->
    Val = test(Pids),
    case Val == State of
        true ->
            erlang:yield(),
            Val2 = test(Pids),
            case Val2 == State of
                true ->
                    {ok, Val2};
                false ->
                    fixpoint(Pids, Val2)
            end;
        false ->
            erlang:yield(),
            fixpoint(Pids, Val)
    end.

test(Pids) ->
    error_logger:info_report([{pids, Pids}]),
    [test_pid(P) || P <- Pids].

test_pid(P) ->
    case process_info(P, status) of
        undefined ->
            {P, dead, 0};
        {status, St} ->
            {reductions, Reds} = process_info(P, reductions),
            case St of
                waiting ->
                    {P, waiting, Reds};
                _Other ->
                    {P, make_ref, Reds}
            end
    end.
