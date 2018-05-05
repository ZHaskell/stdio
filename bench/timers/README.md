Benchmark for different Disk IO
===============================

High performance timers are always the base for I/O libraries. Although base provide high precision timers based on min heap and OS's timers, the performance is not satisfactory when there's a large quantity of timeouts. This test benchmark our new low resolutin timers based on timing wheel with the one in base.

Run test
--------

```
cabal build
./dist/build/system-timer/system-timer +RTS -s
./dist/build/low-res-timer/low-res-timer +RTS -s
```
