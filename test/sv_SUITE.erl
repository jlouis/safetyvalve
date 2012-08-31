-module(sv_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0,
	 init_per_group/2, end_per_group/2,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([ping/1]).

suite() ->
    [{timetrap, {minutes, 3}}].

%% Setup/Teardown
%% ----------------------------------------------------------------------
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_suite(Config) ->
    [ok = application:start(App) ||
        App <- [syntax_tools, compiler, lager, safetyvalve]],
    Config.

end_per_suite(_Config) ->
    [ok = application:stop(App) ||
        App <- lists:reverse([syntax_tools, compiler, lager, safetyvalve])],
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%% Tests
%% ----------------------------------------------------------------------
groups() ->
    [{basic, [shuffle], [ping]}].

all() ->
    [{group, basic}].

ping(_Config) ->
    ok.
