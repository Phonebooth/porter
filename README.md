# porter

`porter` is a simple Erlang library that is makes creating many client tcp
connections easier in Linux. Its primary function is to keep track of source ports
used per remote address and safely allocate new ones for new connections.

https://idea.popcount.org/2014-04-03-bind-before-connect/
