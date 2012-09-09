%% A Simple worker process which tries to do work.
-module(worker).

-export([start_link/0]).

start_link() ->
    proc_lib:spawn_link(fun do_work/0).

do_work() ->
    case sv:run(test_queue_1,
                fun () ->
                        done = manager:doing_work()
                end) of
        done -> manager:status(done);
        {error, _Reason} = Err -> manager:status(Err)
    end.

