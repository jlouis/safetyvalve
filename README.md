# Safety Valve

The Safety Valve Erlang application provides overload protection for
Erlang systems. It provides queueing facilities for tasks to be
executed so their concurrency and rate can be limited on a running
system.

# Inspiration

Safetyvalve owes its inspiration to Ulf Wigers `jobs` framework, but
it is a different implementation and a different approach as well. The
main difference is that safetyvalve serves as a simpler solution (for
now).

One goal is to employ QuickCheck to prove certain properties of
Safetyvalve over time. This was hard with `jobs` - but this tool
should be designed around the idea that it can be tested more.

# Status

Safety Valve is still under development

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
QDef = {my_queue, [{hz, 1000}, % Poll the queue every 1000 ms
                   {rate, 5},  % Produce 5 requests per poll
                   {token_limit, 15}, % Allow a 15 token burst
                   {size, 60}, % Keep at most 60 tasks waiting
                   {concurrency, 3}] % Start at most 3 jobs simultaneously
```

The configuration will tell the queue to poll once every 1000ms via
the `hz` value. Setting this value lower makes the queue reconsider
its tokens more often, with a less jagged performance as a result.
Setting this value too low may however make your Erlang VM spend a
high amount of time in a poller loop, doing essentially nothing.

The `rate` is the number of tokens to add per poll.

The `token_limit` configures how many tokens there can be in the
bucket at a given point in time. This allows you to "burst" out
quickly in the beginning.

The `size` parameter configures the size of the queue (*NOTE*:
Currently we can't honor this).

The `concurrency` parameter configures how many concurrent jobs/tasks
this queue will allow once a task has gotten the "go" signal. (*NOTE*:
Currently we ignore this value).

Then, to get jobs onto this queue, do the following

```
Res = sv:run(my_queue, fun work/0)
```

The result value `Res` will either be the output of the `work/0`
function or the tuple `{error, Reason}` if there is some overload
condition preventing it from running.

# License

MIT

