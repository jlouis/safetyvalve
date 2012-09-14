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

test_pid(Pid) ->
    case test_status(test_reductions(Pid)) of
        {Pid, State, Reds} ->
            {Pid, State, Reds};
        undefined ->
            {Pid, undefined, undefined}
    end.

test_reductions(Pid) ->
    case process_info(Pid, reductions) of
        {reductions, Reds} ->
            {Pid, Reds};
        undefined ->
            {Pid, undefined}
    end.

test_status({_, undefined}) -> undefined;
test_status({Pid, Reds}) ->
    case process_info(Pid, status) of
        undefined ->
            undefined;
        {status, waiting} ->
            {Pid, waiting, Reds};
        _Other ->
            {Pid, make_ref, Reds}
    end.
