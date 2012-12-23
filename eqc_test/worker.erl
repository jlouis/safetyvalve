%% A Simple worker process which tries to do work.
-module(worker).

-export([start/1, start_link/1]).

start(TimePoint) ->
    proc_lib:spawn(fun () -> do_work(TimePoint) end).

start_link(TimePoint) ->
    proc_lib:spawn_link(fun () -> do_work(TimePoint) end).

do_work(TimePoint) ->
    case sv:run(test_queue_1, TimePoint,
                fun () ->
                            done = manager:doing_work()
                end) of
        {ok, done} -> manager:status(done);
        {error, _Reason} = Err -> manager:status(Err)
    end.

