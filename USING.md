# An example of using the safetyvalve system

Let us say we have a system like the epgsql driver,

	https://github.com/wg/epgsql
	
This driver has no connection pooling, so we can write a system to do this. Basically, we keep a pool of connections and then when we want a connection, we request it from the pool. If there is no connection in the pool, then we create a new one.

This will work, but it has the problem that if we suddenly get too many processes that needs to access the database, then we will overload it. Postgresql has a limit on how many connections we allow to it. So we want to use safetyvalve to do this.

First, we create a queue. This is done by configuring the safetyvalve application in `sys.config` or similar place.

	{safetyvalve,
		{queues, [{pg_q, [{hz, 500}, {rate, 20}, {token_limit, 30}, {size, 30}, {concurrency, 32}]}]}},
		
This says the following:

* The queue has a rate-limit that generates up to 20 new connections every half second. The token-limit says there will at most be 30 tokens waiting, if no workers are doing work.
* The concurrency level is 32, which means there will at most be 32 active connections to the Postgres database. This is what makes sure we cannot overload the database system.
* The queue size is 30. So if there are too many workers, we allow up to 30 processes to wait around for a connection. If a process is #31 in the queue, then the process is rejected and will be told that the Postgres system is overloaded. The process can then itself make a choice of what to do. It may decide to wait a bit or fail permanently.

Let us say we have the following:

	with_pg(QueryFun) ->
	      {ok, C} = pg_pool:obtain_connection(),
	      QueryFun(C),
	      pg_pool:putback(C).
	      
which executes QueryFun in the context of a Connection. We assume the pool has a monitor on this process so it knows if the `QueryFun` dies.

The only thing you need to change in order to make safetyvalve used here, is that you need to start the safetyvalve application first. This can best be done in your applications `.app` file by having it as a dependency and then have a release which boots it. Safetyvalve will pick up the queue definition of `pg_q` and arrange to start up this queue for us. So we can use that queue:

	run_query(QueryFun) ->
	    case sv:run(fun() -> with_pg(QueryFun) end) of
	      {ok, Res} -> {ok, Res};
	      {error, Reason} -> {error, Reason}
	    end.
	 
If we get to run, we will return `{ok, Res}` where Res is the result from the query. If it fails, what will be returned is `{error, Reason}`. The usual `Reason` is the atom `queue_full` which will tell you that there are 32 working on the database and that there are 30 in queue to work. This is where we will reject the job.

What to do when a job is rejected is up to you. Either you can reject the job upwards. If you are using this to limit certain in-bound HTTP requests, you can return a 503 to designtate to the caller we have overload. Or you can wait for some time and try again and retry. Whenever you retry, you can increase a backoff. For real-time queries, it is usually best to reject and fail the request quickly. A user is usually not going to wait. For a batch-job, it is usually better to wait and try again.

