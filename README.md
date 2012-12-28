# Safety Valve

The Safety Valve Erlang application provides overload protection for
Erlang systems. It provides queueing facilities for tasks to be
executed so their concurrency and rate can be limited on a running
system.

# Using

This project uses semantic versioning. Tags are declared like `vX.Y.Z`
(do note the `v`). The meaning is:

* X changes: There have been backwards-incompatible changes to the `sv` API functions which are meant to be the only functions exported. Or there has been changes to the way the configuration file format is specified.
* Y changes: These are additions to the API, but existing functionality has not been broken.
* Z changes: Bug fixes.

See the document USING.md for a usage example.

# Changes

### v2.0.0 -> v2.2.0

Allow to pass parameters to queue algorithms. In particular, use this to provide parameters to the CoDel
classifier. Change should be fully backwards compatible.

### v2.0.0 -> v2.1.0

Enable a new configuration parameter, `queue_type`. This parameter defines a module which handles the queueing strategy used by the system. In short, queues are now plugable. If omitted, the queue defaults to `sv_queue_ets` which is an ETS-based queueing strategy. Hence, the minor version bump as it should not affect any users and be fully backwards compatible.

### v1.0.0 -> v2.0.0

Remove `sv:run/3` and replace it with `sv:run/2` for now. The return value is still changed according to the changes in v1.0.0. This change is done to hide certain internal structure for now until we get it fleshed out more. It also enables us to do more advanced queueing strategies.

### v0.1.0 -> v1.0.0

The return value of `sv:run/3` changed from `Res | {error, Reason}` to `{ok, Res} | {error, Reason}`. This better reflects the system and we can distinguish between an error term from the function we run and safetyvalve itself.

# Inspiration

Safetyvalve owes its inspiration to Ulf Wigers `jobs` framework, but
it is a different implementation and a different approach as well. The
main difference is that safetyvalve serves as a simpler solution (for
now).

One goal is to employ QuickCheck to prove certain properties of
Safetyvalve over time. This was hard with `jobs` - but this tool
should be designed around the idea that it can be tested more.

# Status

Safety Valve is still under development. The current state is that we
have a quickcheck model for the following configuration (it is
described below what this configuration means):

```
QDef = {my_queue, [{hz, undefined},
                   {rate, 1..5},
                   {token_limit, 1..5},
                   {size, 1..5},
                   {concurrency, 1..5 }]}
```

Where the value `1..5` means `choose(1,5)` in QuickCheck terminology. That is, we
randomly select a value between 1 and 5 (inclusive).

For this configuration we have passed the QuickCheck model. While this
does not prove the system correct, it does argue most of the system
has been tested.

# Configuration

To configure a safety valve for your project, add the following
section to your sys.config

```
{safetyvalve,
  [{queues, [QDef, QDef, ...]}]}
```

Where each `QDef` is a queue definition of a queue. For now all queues
are Token Bucket Regulators with a rate limit and a poll frequency:

```
QDef = {my_queue, [{queue_type, sv_queue_ets},
                   {hz, 1000}, % Poll the queue every 1000 ms
                   {rate, 5},  % Produce 5 requests per poll
                   {token_limit, 15}, % Allow a 15 token burst
                   {size, 60}, % Keep at most 60 tasks waiting
                   {concurrency, 3}]} % Start at most 3 jobs simultaneously
```

The most important flag is the `queue_type` which sets up what type of queue
we want to run. Currently, you can pick among the following:

* `sv_queue_ets` - An ETS based queue. Jobs are stored in an `ordered_set` ETS table as `{Timestamp, Job}` and are picked by calling `ets:first/1`.
* `sv_codel` - A CoDel classifier (Controlled Delay - pronounced 'coddle'). CoDel is a *parameterless*, *burst permitting* delay controller. the CoDel algorithm targets a given wanted latency for a job in queue and begins rejecting jobs if that latency is not met. Also, it has hysteresis-like capabilities which allows the algorithm to handle sudden bursts of traffic. **CoDel is currently experimental**.
* `{sv_codel, [Target, Interval]}` - A CoDel classifier with tuned parameters. The `Target` parameter determines the desired queue latency in ms. The default is 5ms, used by the parameterless CoDel classifier. The `Interval` parameter defines the window before CoDel should begin dropping packets. The default is 100ms. So if we have been above the target for interval ms, then the queue algorithm will begin dropping. For some jobs, however, it may be desirable to tune these parameters up. **CoDel is currently experimental**.

The rest of the configuration will tell the queue to poll once every 1000ms via
the `hz` value. Setting this value lower makes the queue reconsider
its tokens more often, with a less jagged performance as a result.
Setting this value too low may however make your Erlang VM spend a
high amount of time in a poller loop, doing essentially nothing. It is
also possible to specify `undefined` in which case it will never poll.
You have to call `sv_queue:poll(QName)` to manually poll. This is very
useful for testing!

* The `rate` is the number of tokens to add per poll.
* The `token_limit` configures how many tokens there can be in the
  bucket at a given point in time. This allows you to "burst" out
  quickly in the beginning.
* The `size` parameter configures the size of the queue
* The `concurrency` parameter configures how many concurrent jobs/tasks
  this queue will allow once a task has gotten the "go" signal.

Then, to get jobs onto this queue, do the following

```
Res = sv:run(my_queue, fun work/0)
```

The result value `Res` will either be the output of the `work/0`
function or the tuple `{error, Reason}` if there is some overload
condition preventing it from running.

# License and Copyright

The source code is Copyright Erlang Solutions Ltd.

It is licensed under the Apache 2.0 License. See the file `LICENSE`
in the repository for the details of this license.

