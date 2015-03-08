%%% @doc Implementation of a Table of waiters for Safetyvalve
%%% This module implements what I call a `Table' which is a table of waiters
%%% which are waiting to run on the node. It could be called a `queue' and
%%% it was earlier on, but the problem is that this stack is both a LIFO and a
%%% FIFO.
%%%
%%% The table here is implemented via ETS as a lookup table. The tuples we store
%%% are of the form `{Timestamp, Element}' where Timestamp is the point where the
%%% element entered the queue. We then utilize the fact that we configure the ETS
%%% table as being `ordered_set' so we can quickly pick out the front and back.
%%%
%%% Currently, the queue here is implemented in FIFO order, but later it would make
%%% sense to configure the queue such that it can be run in LIFO order as well.
%%%
%%% The Table has an important assumption: There can at most be a single entry per time-stamp.
%%% This means you have to produce unique timestamps. Otherwise the queue implementation
%%% will fail to operate correctly.
%%% 
%%% The interface is deliberately kept "functional" in style. It returns the Queue Name so the code
%%% can be used as a replacement for the `queue' module. It also tries to implement the same API
%%% so the substitution is as easy as possible.
%%% @end
-module(sv_queue_ets).

-export([new/0, delete/1]).

-export([out/2, len/1, in/3, remove/3]).

new() ->
    ets:new(queue, [protected, ordered_set]).
	
delete(Q) -> ets:delete(Q).

out(_Ts, QName) ->
    case ets:first(QName) of
        '$end_of_table' -> {empty, [], QName};
            Key ->
            [{_T, E} = Obj] = ets:lookup(QName, Key),
            true = ets:delete_object(QName, Obj),
            {E, [], QName}
    end.
	
len(QName) ->
    ets:info(QName, size).
	
%% Format is kept like this to make sure it follows that of the `queue' module.
in(Item, TS, QName) ->
    true = ets:insert_new(QName, {TS, Item}),
    QName.

remove(Item, TS, QName) ->
    true = ets:delete_object(QName, {TS, Item}),
    QName.
