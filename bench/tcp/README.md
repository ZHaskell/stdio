Benchmark for new libuv I/O manager
===============================

This benchmark compares following I/O multiplexers:

+ current one in base, aka. mio     

This is an M:N multiplexers, each OS thread(capability in GHC rts) use a kqueue/epoll fd to do event polling, and one haskell thread to manager the poller.

+ libuv I/O manager in stdio

This is an M:N multiplexers just like mio, but use libuv as OSes abstraction, each OS thread(capability in GHC rts) use an `uv_loop` poller, and one haskell thread to mananger the poller.

+ golang's netpoller

This is an M:N multiplexers, but golang rts only start one extra thread doing I/O multiplex, M user threads on N OS threads all request I/O scheduling from this poller thread.

+ nodejs cluster

This is a single threaded multiplexers, but use multiples process to take advantage of multiple CPU.


Run test
--------

This benchmark will start a server on your localhost's 8888 port(or use PORT environment varible if available), read some input(and ignore them), them servering 500 bytes of zeros in HTTP protocal, so that you can use HTTP benchmark tools such as `siege` or `wrk` to bench. A small respond size is choosen to highlight overhead each multiplexer added.

You should adjust your system's fd limit before running benchmark in case of running out of fd.

```
cabal build

# Adding a proper heap size hint is important for haskell programs because
# the way GHC's GC works.
# You should use a -Hx parammeter if the concurrent level go beyong ~1k.
# Quick formula: concurrent level(in K) * 10M, for example use -H128M for C10K.

# mio, if you know your CPU's core number x, append a -Nx
./dist/build/mio/mio +RTS -s

# stdio, if you know your CPU's core number x, append a -Nx
./dist/build/libuv/libuv +RTS -s

# golang
go run golang/main.go

# nodejs, if you know your CPU's core number x, set it with env CPU_NUM=x
node nodejs/main.js

# wrk
wrk -c1000 -d10s http://127.0.0.1:8888   

# siege
siege -c 1000 -r 10 http://127.0.0.1:8888 

# ab
ab -r -k -c 100 -n 30000 http://127.0.0.1:8888/
...
```
