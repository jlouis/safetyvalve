# Using the safetyvalve system

The first question to ask oneself is if safetyvalve is at all a tool you need. The price you are paying with safetyvalve in its current setup is that every request must factor through the `sv_queue` process and this will hurt performance when you are running very fast.

There is a tool which can be used in many situations which is simple and efficient. Whenever you want to run a process, you ask the system statistics for the current load. If the load is above a threshold, you deny the job. This is a simple and efficient measure, it can be made to run and a highly concurrent fashion, it is independent of the underlying machine. And so on. Basically, you write a process which measures the current load on the system and you then stop serving new requests if the load is currently too high.

While this method works, it does have a couple of weaknesses. One is that you have no queueing, so you cannot take sudden small spiky bursts in traffic. Also, while this is effective when you have 400.000 connections and more, it doesn't really work if the resources you are looking at are severely constrained. Nor does it work if the work you are going to do are heavyweight.

Another thing to think about is the duration of the task you want to execute. In a telephony switch, a call taken is going to last a minute easily on average, perhaps even more. This means that taking a new call on is a long-lived task. In a web server on the other hand, a task can often be solved in milliseconds. This means that the current amount of concurrent work will dissipate very quickly and thus it is perhaps more okay to queue requests for a little while.

## The purpose of a queue

Why do we add queues to our system? The main reason is to smooth out spikes. That is, if requests enter the system very quickly, we don't want to process all requests just now, but rather take them in a bit at a time at a given well defined rate where we know we can handle the load. If we get bombed with 1000 requests in 2 ms, we might not want to make 1000 requests to a backend system. While Erlang can naturally handle the load, it might not be the case for the database you call. This is the purpose of the queue. It lets us handle all the requests, if the requests are willing to wait around a bit.

One should also beware queues though. Since new requests enter the back of the queue, they have to travel through the queue before they can get service. The time through the queue—the sojourn time—is added latency on the request. If your requests are of batch-nature, you don't care about this time. You just want all requests processed eventually and the time you are willing to accept may be 24 hours or even more. On the contrary, if you are serving real-time requests, the acceptable sojourn time may be measured in milliseconds. If a customer is waiting for the request, you can't let them wait too long. Otherwise they will live before you can produce a satisfactory answer.

This means you cannot just add a queue of infinite size in the realtime scenario. Rather you must cap the size of the queue and decide what to do when the queue runs full. Or when the sojourn time in the queue becomes too high.

Safetyvalve provides a `queue_size` parameter for the first case. And provides a `CoDel` classifier for the second case. CoDel will leave the queue alone until the sojourn time goes bad. Then it will begin dropping work from the queue until it can get the sojourn time down to acceptable levels again.

## Typical use cases:

Safetyvalve works best in some specific cases:

* A task is going to use some heavily limited resource. You may only have 30 database connections open on some database systems for instance. And adding more might not be smart since you will have worse performance for everyone due to heavy disk seeks.
* A task will use a lot of memory when it runs. Therefore, you need an upper bound on the amount of these tasks that are running at the same time.
* A task is CPU-bound. Hence you would rather like to have relatively few of them and having them finish; Not thousands running at the same time.
* You are going to make HTTP requests to another system and that system is known to be slow to respond and easy to overload.

## Typical non-use cases:

* You want to go as fast as possible. It is better to ask about scheduler usage or process count in this case. It lets individual processes run concurrently and does not impose a limit by the `sv_queue` process.
* You are more concerned about a specific client overloading your system. That is, you want to run an intensity model for a given user. In this case, it is often better to hash the client into a bucket and keep an intensity regulation on the bucket. Note is is enough to have a decay algorithm where you store a pair of `{Intensity, Timestamp}` and update according to a decay parameter. Inspiration might be found in the OTP module `overload`.
* You want to queue clients *and* are interested in per-client load. Again, hashing into buckets and running an SFQ over CoDel is a patch that might get in later, but it is not supported right now.
* All jobs started *must* finish. Load regulation means that we throw away work in the overload situation.
* You need load regulation feedback. Currently, we don't sample the system and impose further constraints on queues when the system is highly loaded.

# An example of using the safetyvalve system

Let us say we have a system like the epgsql driver,

	https://github.com/wg/epgsql
	
This driver has no connection pooling, so we can write a system to do this. Basically, we keep a pool of connections and then when we want a connection, we request it from the pool. If there is no connection in the pool, then we create a new one.

This will work, but it has the problem that if we suddenly get too many processes that needs to access the database, then we will overload it. Postgresql has a limit on how many connections we allow to it. So we want to use safetyvalve to do this.

First, we create a queue. This is done by configuring the safetyvalve application in `sys.config` or similar place.

	{safetyvalve,[
		{queues, [{pg_q, [{hz, 500}, {rate, 20}, {token_limit, 30}, {size, 30}, {concurrency, 32}]}]}
	]}
		
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
	    case sv:run(pg_q, fun() -> with_pg(QueryFun) end) of
	      {ok, Res} -> {ok, Res};
	      {error, Reason} -> {error, Reason}
	    end.
	 
If we get to run, we will return `{ok, Res}` where Res is the result from the query. If it fails, what will be returned is `{error, Reason}`. The usual `Reason` is the atom `queue_full` which will tell you that there are 32 working on the database and that there are 30 in queue to work. This is where we will reject the job. Note the mention of `pg_q` in there which designates what queue to use. We can have multiple such queues if we have more than a single class of processes.

What to do when a job is rejected is up to you. Either you can reject the job upwards. If you are using this to limit certain in-bound HTTP requests, you can return a 503 to designtate to the caller we have overload. Or you can wait for some time and try again and retry. Whenever you retry, you can increase a backoff. For real-time queries, it is usually best to reject and fail the request quickly. A user is usually not going to wait. For a batch-job, it is usually better to wait and try again.

