-module(eqc_helpers).

-export([fixpoint/1]).


fixpoint(Pids) ->
    fixpoint(Pids, test(Pids)).

fixpoint(Pids, State) ->
    Val = test(Pids),
    case Val == State of
        true ->
            timer:sleep(3),
            Val2 = test(Pids),
            case Val2 == State of
                true ->
                    {ok, Val2};
                false ->
                    fixpoint(Pids, Val2)
            end;
        false ->
            timer:sleep(2),
            fixpoint(Pids, Val)
    end.

test(Pids) ->
    [test_pid(P) || P <- Pids].

test_pid(P) ->
    {status, Status} = process_info(P, status),
    {reductions, Reds} = process_info(P, reductions),
    case Status of
        waiting ->
            {P, waiting, Reds};
        _Other ->
            {P, make_ref, Reds}
    end.
            
        
    
