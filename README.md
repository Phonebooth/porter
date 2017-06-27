# porter

`porter` is a simple Erlang library that is makes creating many client tcp
connections easier in Linux. Its primary function is to keep track of source ports
used per remote address and safely allocate new ones for new connections.

The following blog post does a great job explaining why you need to manage your
own local ports if you're creating many outgoing TCP connections from a single
Linux server.

https://idea.popcount.org/2014-04-03-bind-before-connect/

In short, you must use SO_REUSEADDR, and the kernel's port allocation algorithm
does not allocate ports efficiently when using SO_REUSEADDR.

## Creating a connection with porter
The `porter` module provides `porter:connect/5` for creating tcp and ssl connections,
with arguments:
   * `Module`: `gen_tcp` or `ssl`
   * `IP`: ipv4 remote address
   * `Port`: remote port
   * `Opts`: options passed into `Module:connect`
   * `Timeout`: timeout passed into `Module:connect`
   
Its return is `{ok, Socket, PorterState}` or `{error, Reason}`. The caller must
keep track of PorterState during the lifetime of the socket. If the caller intends
to initiate the connection close, you must use `porter:close/3`. If the connection
is closed by the remote, you must call `porter:on_closed/1`. These functions ensure
the allocated local port is released back into the pool.

## Local port release and `tcp_tw_reuse`
When a local port is released (connection is closed), it enters an aging period before
it's fully available for a new `porter` connection. We want to give the socket some
period in the TIME_WAIT state, if necessary, to cleanly shutdown. However, the aging
period is _shorter_ than the TIME_WAIT interval, 60 seconds. Therefore, it's strongly
suggested that you use `tcp_tw_reuse` on your server so that porter's short aging of the
local port does not create collisions with existing TIME_WAIT sockets in the kernel.

## Configuration
You must define the following environment variables for porter to operate correctly:
  * `ip_local_port_range`: porter does not attempt to read the ip_local_port_range from your
  Linux system configuration, so you must provide it explicitly. Default is `{1024, 65535}`
  * `retry_policy`: If a connection attempt fails with eaddrnotavail or eaddrinuse, `porter`
  will optionally try again with a new local port allocation. This is to meant to protect
  against collisions with other local ports in use on the system that may be unrelated to
  `porter`s management of local ports. An example `retry_policy` is `[{attempts, 10}]`, which
  will try 10 different local ports before giving up. The default `retry_policy` is `[]`,
  for which `porter` takes no action on a failed connection.
  
## Internals
Local ports are tracked in several ets tables. The master table is public and named,
called `porter_addr_map`, so it can be dumped at any time with `ets:tab2list(porter_addr_map).`

The `porter_addr_map` contains permanent `pool` entries and transient `aging` entries, one entry
per remote addr, remote port combination. Each entry in the table is a 2-tuple, where the second
value is a handle to another ets table. For example: the record `{{pool,{10,1,4,148},8883},1017980}`
describes a local port pool for `10.1.4.148:8883` and the pool contents are located in ets
table `1017980`, viewable with `ets:tab2list(1017980).`.
