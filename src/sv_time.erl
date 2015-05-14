-module(sv_time).

-export([monotonic_time/1, unique_integer/0]).

%% This file is allowed to use deprecated functions, since it is a compatibility layer
-compile(nowarn_deprecated_function).

monotonic_time(Unit) ->
    try
	erlang:monotonic_time(Unit)
    catch
	error:badarg ->
	    erlang:error(badarg, [Unit]);
	error:undef ->
	    %% Use Erlang system time as monotonic time
	    STime = erlang_system_time_fallback(),
	    try
		convert_time_unit_fallback(STime, native, Unit)
	    catch
		error:bad_time_unit -> erlang:error(badarg, [Unit])
	    end
    end.

unique_integer() ->
    try
	erlang:unique_integer()
    catch
	error:undef ->
	    {MS, S, US} = erlang:now(),
	    (MS*1000000+S)*1000000+US
    end.

%% Internal functions
%% -----------------------------------------------
erlang_system_time_fallback() ->
    {MS, S, US} = erlang:now(),
    (MS*1000000+S)*1000000+US.

integer_time_unit(native) -> 1000*1000;
integer_time_unit(nano_seconds) -> 1000*1000*1000;
integer_time_unit(micro_seconds) -> 1000*1000;
integer_time_unit(milli_seconds) -> 1000;
integer_time_unit(seconds) -> 1;
integer_time_unit(I) when is_integer(I), I > 0 -> I;
integer_time_unit(BadRes) -> erlang:error(bad_time_unit, [BadRes]).

convert_time_unit_fallback(Time, FromUnit, ToUnit) ->
    FU = integer_time_unit(FromUnit),
    TU = integer_time_unit(ToUnit),
    case Time < 0 of
	%% true -> TU*Time - (FU - 1); % Time can't be negative in the current code!
	false -> TU*Time
    end div FU.
