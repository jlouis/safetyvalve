-module(sv_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0,
	 init_per_group/2, end_per_group/2,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([ping/1, through/1, many_through_ets/1, many_through_codel/1]).

suite() ->
    [{timetrap, {seconds, 30}}].

%% Setup/Teardown
%% ----------------------------------------------------------------------
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_suite(Config) ->
    application:load(safetyvalve),
    Queues = [
      {test_queue_1_ets, [{hz, 1000}, {rate, 5}, {token_limit, 15}, {size, 60}, {concurrency, 3}]},
      {test_queue_1_codel, [{hz, 200}, {rate, 1}, {token_limit, 15}, {size, 60}, {concurrency, 3},
      	{queue_type, {sv_codel, [5, 100]}}]}
    ],
    application:set_env(safetyvalve, queues, Queues),
    {ok, _Apps} = application:ensure_all_started(safetyvalve),
    Config.

end_per_suite(_Config) ->
    application:stop(safetyvalve),
    ok.

init_per_testcase(many_through_codel, Config) ->
    %% sv_tracer:start_link(filename:join(?config(priv_dir, Config), "trace.out")),
    Config;
init_per_testcase(not_applicable, Config) ->
    dbg:tracer(),
    dbg:tpl({sv_queue_ets, in, 2}, cx),
    dbg:tpl({queue, in, 2}, cx),
    dbg:p(whereis(test_queue_1), [c]),
    Config;
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(many_through_codel, _Config) ->
    sv_tracer:stop(),
    ok;
end_per_testcase(not_applicable, _Config) ->
    dbg:stop(),
    ok;
end_per_testcase(_Case, _Config) ->
    ok.

%% Tests
%% ----------------------------------------------------------------------
groups() ->
    [{basic, [shuffle], [
      many_through_codel,
      many_through_ets,
      ping,
      through
    ]}].

all() ->
    [{group, basic}].

ping(_Config) ->
    ok.

through(_Config) ->
    {ok, ok} = sv:run(test_queue_1_ets, fun work/0).

many_through_ets(_Config) ->
    Parent = self(),
    Pids = [
      begin
        Pid = spawn_link(fun() ->
    	  case sv:run(test_queue_1_ets, fun work/0) of
    	      {ok, ok} -> Parent ! {done, self()};
    	      {error, overload} -> Parent ! {overload, self()}
    	  end
        end),
        timer:sleep(20),
        Pid
      end || _ <- lists:seq(1, 60)],
    {ok, Overloads} = collect(Pids, 0),
    ct:log("Overloads: ~B", [Overloads]),
    true = Overloads == 0.

many_through_codel(_Config) ->
    Parent = self(),
    Pids = [
      begin
        Pid = spawn_link(fun() ->
          case sv:run(test_queue_1_codel, fun work/0) of
              {ok, ok} -> Parent ! {done, self()};
              {error, overload} -> Parent ! {overload, self()}
          end
        end),
        timer:sleep(20),
        Pid
      end || _ <- lists:seq(1, 60)],
    {ok, Overloads} = collect(Pids, 0),
    ct:log("Overloads: ~B", [Overloads]),
    true = Overloads > 0.

%% ----------------------------------------------------------------------
collect([], Overloads) ->
    {ok, Overloads};
collect(Pids, Overloads) when is_list(Pids) ->
    receive
        {done, Pid} ->
            collect(Pids -- [Pid], Overloads);
        {overload, Pid} ->
            collect(Pids -- [Pid], Overloads + 1)
    after 5000 ->
            {error, {timeout, Pids, Overloads}}
    end.

    

work() ->
    timer:sleep(30),
    ok.
