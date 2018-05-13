Benchmark machine
-----------------

Thanks @treevivi for sharing his machines (and my company @bytedance of course).

The load generator have more cores(64) than server(48) to guarantee we can generate enough load. I also made some summary charts [here](https://github.com/haskell-stdio/stdio/blob/master/bench/tcp/result-summary.pdf), only throughput data are covered.

+ workload generator

    ```
    OS: Linux n15-033-209 3.16.0-4-amd64 #1 SMP Debian 3.16.43-2+deb8u5 (2017-09-19) x86_64 GNU/Linux
    CPU: Intel(R) Xeon(R) CPU E5-2683 v4 @ 2.10GHz
    MEM: 131921900 kB

    ulimit -n: 1024768 
    eth0: Speed: 10000Mb/s
    ```

+ test server

    ```
    OS: Linux n20-016-154 3.16.0-4-amd64 #1 SMP Debian 3.16.43-2+deb8u5 (2017-09-19) x86_64 GNU/Linux
    CPU: Intel(R) Xeon(R) CPU E5-2650 v4 @ 2.20GHz
    Mem: 263771248 kB

    ulimit -n: 1024768 
    eth0: Speed: 10000Mb/s
    ```

Golang Result
-------------

+ wrk -t64 -c 100 -d 60s

    + GOMAXPROCS=1

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   349.99us  182.60us  33.21ms   73.57%
            Req/Sec     2.87k   257.18     3.89k    68.64%
          10985661 requests in 1.00m, 6.18GB read
        Requests/sec: 182791.14
        Transfer/sec:    105.29MB
    ```

    + GOMAXPROCS=4

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   144.53us  684.37us 201.65ms   99.98%
            Req/Sec     7.11k   555.54     8.65k    66.80%
          27210227 requests in 1.00m, 15.31GB read
        Requests/sec: 452754.09
        Transfer/sec:    260.80MB
    ```

    + GOMAXPROCS=10

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   134.28us  682.68us 202.15ms   99.98%
            Req/Sec     7.65k   683.98     9.23k    67.40%
          29291692 requests in 1.00m, 16.48GB read
        Requests/sec: 487387.83
        Transfer/sec:    280.74MB
    ```
        
    + GOMAXPROCS=20

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   129.68us  699.81us 201.54ms   99.96%
            Req/Sec     8.01k   748.03     9.51k    63.29%
          30661524 requests in 1.00m, 17.25GB read
        Requests/sec: 510173.78
        Transfer/sec:    293.87MB
    ```

    + GOMAXPROCS=40

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   147.15us  112.65us  16.84ms   92.24%
            Req/Sec     6.92k   519.14     8.62k    67.27%
          26479432 requests in 1.00m, 14.90GB read
        Requests/sec: 440589.54
        Transfer/sec:    253.79MB
    ```

+ wrk -t64 -c 1000 -d 60s

    + GOMAXPROCS=1

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     6.01ms    5.49ms 199.52ms   99.57%
            Req/Sec     2.61k   386.30    38.14k    99.70%
          9973999 requests in 1.00m, 5.61GB read
        Requests/sec: 165957.03
        Transfer/sec:     95.59MB
    ```

    + GOMAXPROCS=4

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     1.93ms    2.75ms 205.87ms   99.84%
            Req/Sec     8.20k     0.90k   62.88k    82.16%
          31351043 requests in 1.00m, 17.64GB read
        Requests/sec: 521653.85
        Transfer/sec:    300.48MB
    ```

    + GOMAXPROCS=10

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     0.92ms    1.37ms  85.20ms   99.25%
            Req/Sec    17.30k     3.96k   56.34k    76.82%
          66176889 requests in 1.00m, 37.23GB read
        Requests/sec: 1101125.26
        Transfer/sec:    634.27MB
    ```

    + GOMAXPROCS=20

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     0.94ms    2.68ms 136.13ms   98.53%
            Req/Sec    19.09k     7.66k   76.24k    80.90%
          73011753 requests in 1.00m, 41.07GB read
        Requests/sec: 1214851.34
        Transfer/sec:    699.78MB
    ```

    + GOMAXPROCS=40

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   791.44us    1.82ms 112.81ms   99.85%
            Req/Sec    20.52k     8.14k   62.41k    53.22%
          78514774 requests in 1.00m, 44.17GB read
        Requests/sec: 1306399.43
        Transfer/sec:    752.51MB
    ```

+ wrk -t64 -c 5000 -d 60s

    + GOMAXPROCS=1
        
    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    45.11ms    8.93ms 269.03ms   84.42%
            Req/Sec     1.75k   412.32    44.71k    84.76%
          6669235 requests in 1.00m, 3.75GB read
        Requests/sec: 110968.52
        Transfer/sec:     63.92MB
    ```

    + GOMAXPROCS=4
    
    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    12.02ms    7.72ms 301.61ms   99.39%
            Req/Sec     6.72k     1.23k  172.04k    99.74%
          25670271 requests in 1.00m, 14.44GB read
        Requests/sec: 427124.80
        Transfer/sec:    246.03MB
    ```

    + GOMAXPROCS=10

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     5.97ms    6.46ms 271.67ms   99.70%
            Req/Sec    13.78k     1.69k  165.01k    99.13%
          52648120 requests in 1.00m, 29.62GB read
        Requests/sec: 876010.01
        Transfer/sec:    504.60MB
    ```

    + GOMAXPROCS=20

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    15.78ms   44.36ms 881.53ms   92.20%
            Req/Sec    20.24k     5.47k  170.54k    76.09%
          77227115 requests in 1.00m, 43.44GB read
        Requests/sec: 1284988.84
        Transfer/sec:    740.18MB
    ```

    + GOMAXPROCS=40

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    24.26ms   58.61ms 868.70ms   88.54%
            Req/Sec    22.17k     7.07k  187.24k    78.67%
          84518152 requests in 1.00m, 47.54GB read
        Requests/sec: 1406300.94
        Transfer/sec:    810.06MB
    ```

+ wrk -t64 -c 10000 -d 60s

    + GOMAXPROCS=1

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    95.89ms    7.74ms 313.36ms   87.02%
            Req/Sec     1.63k   248.03     3.16k    89.76%
          6241191 requests in 1.00m, 3.51GB read
        Requests/sec: 103847.75
        Transfer/sec:     59.82MB
    ```

    + GOMAXPROCS=4

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    25.32ms    8.99ms 365.73ms   99.15%
            Req/Sec     6.28k     1.23k  170.71k    99.60%
          23975921 requests in 1.00m, 13.49GB read
        Requests/sec: 398934.68
        Transfer/sec:    229.79MB
    ```

    + GOMAXPROCS=10

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    74.87ms  134.48ms   1.70s    89.82%
            Req/Sec    13.97k     5.33k  175.43k    82.38%
          53249286 requests in 1.00m, 29.95GB read
        Requests/sec: 886018.93
        Transfer/sec:    510.36MB
    ```

    + GOMAXPROCS=20

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   101.01ms  171.75ms   1.92s    87.21%
            Req/Sec    17.31k     9.32k  183.22k    76.98%
          65964627 requests in 1.00m, 37.11GB read
          Socket errors: connect 0, read 0, write 0, timeout 159
        Requests/sec: 1097576.97
        Transfer/sec:    632.23MB
    ```

    + GOMAXPROCS=40

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   111.23ms  196.99ms   1.96s    87.21%
            Req/Sec    18.62k    12.01k  196.73k    80.57%
          70094572 requests in 1.00m, 39.43GB read
          Socket errors: connect 0, read 0, write 0, timeout 242
        Requests/sec: 1166312.10
        Transfer/sec:    671.82MB
    ```

Node.js Result
--------------

+ wrk -t64 -c 100 -d 60s

    + CPU_NUM=1

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     0.98ms    1.04ms 105.40ms   99.54%
            Req/Sec     1.05k   163.80     2.05k    84.42%
          4016233 requests in 1.00m, 2.26GB read
        Requests/sec:  66827.49
        Transfer/sec:     38.49MB
    ```

    + CPU_NUM=4

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   239.14us  704.99us 201.21ms   99.97%
            Req/Sec     4.31k   657.34     6.25k    68.86%
          16478057 requests in 1.00m, 9.27GB read
        Requests/sec: 274179.33
        Transfer/sec:    157.93MB
    ```

    + CPU_NUM=10

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   165.90us  832.20us 202.71ms   99.90%
            Req/Sec     6.50k   757.77     8.75k    62.08%
          24879393 requests in 1.00m, 14.00GB read
        Requests/sec: 413970.55
        Transfer/sec:    238.46MB
    ```

    + CPU_NUM=20
        
    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   136.41us   88.10us  26.55ms   98.80%
            Req/Sec     7.41k   705.02     9.80k    66.86%
          28356700 requests in 1.00m, 15.95GB read
        Requests/sec: 471831.54
        Transfer/sec:    271.78MB
    ```
        
    + CPU_NUM=40

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   131.39us  727.04us 201.24ms   99.89%
            Req/Sec     8.16k     0.98k   10.72k    71.52%
          31214758 requests in 1.00m, 17.56GB read
        Requests/sec: 519381.66
        Transfer/sec:    299.17MB
    ```

+ wrk -t64 -c 1000 -d 60s

    + CPU_NUM=1

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    17.88ms   72.11ms   2.00s    99.27%
            Req/Sec     1.18k   250.88     6.51k    96.08%
          4351172 requests in 1.00m, 2.45GB read
          Socket errors: connect 0, read 0, write 0, timeout 419
        Requests/sec:  72471.86
        Transfer/sec:     41.75MB
    ```
    
    + CPU_NUM=4

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     3.60ms    6.96ms 366.23ms   99.70%
            Req/Sec     4.57k   626.05    54.33k    98.92%
          17312957 requests in 1.00m, 9.74GB read
        Requests/sec: 288072.06
        Transfer/sec:    165.94MB
    ```

    + CPU_NUM=10

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     1.47ms    2.90ms 204.89ms   99.80%
            Req/Sec    10.97k     1.30k   60.56k    84.99%
          41673463 requests in 1.00m, 23.44GB read
        Requests/sec: 693406.57
        Transfer/sec:    399.42MB
    ```

    + CPU_NUM=20

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     1.12ms    2.96ms 204.36ms   99.58%
            Req/Sec    14.99k     4.35k   77.11k    91.22%
          56956168 requests in 1.00m, 32.04GB read
        Requests/sec: 947696.04
        Transfer/sec:    545.89MB
    ```
        
    + CPU_NUM=40
    
    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     0.90ms    3.18ms 202.33ms   97.85%
            Req/Sec    24.19k    15.38k   78.41k    74.03%
          91821672 requests in 1.00m, 51.65GB read
        Requests/sec: 1527830.32
        Transfer/sec:      0.86GB
    ```

+ wrk -t64 -c 5000 -d 60s

    + CPU_NUM=1

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    31.27ms   28.81ms   2.00s    99.68%
            Req/Sec     1.93k     1.52k   30.22k    78.25%
          3440606 requests in 1.00m, 1.94GB read
          Socket errors: connect 0, read 0, write 0, timeout 2193
        Requests/sec:  57248.24
        Transfer/sec:     32.98MB
    ```

    + CPU_NUM=4

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    22.26ms   48.36ms   2.00s    99.59%
            Req/Sec     3.88k     1.68k   49.58k    93.66%
          13819945 requests in 1.00m, 7.77GB read
          Socket errors: connect 0, read 0, write 0, timeout 3020
        Requests/sec: 229948.19
        Transfer/sec:    132.45MB
    ```

    + CPU_NUM=10

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    11.67ms   46.62ms   1.63s    99.26%
            Req/Sec     9.25k     3.47k  108.00k    97.36%
          34161771 requests in 1.00m, 19.22GB read
        Requests/sec: 568412.00
        Transfer/sec:    327.42MB
    ```

    + CPU_NUM=20

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     8.01ms   25.18ms 876.60ms   98.82%
            Req/Sec    13.34k     6.85k  210.13k    97.38%
          50231754 requests in 1.00m, 28.26GB read
        Requests/sec: 835794.06
        Transfer/sec:    481.43MB
    ```

    + CPU_NUM=40

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    15.38ms   48.34ms   1.82s    92.75%
            Req/Sec    25.16k    11.26k  239.49k    87.78%
          94626321 requests in 1.00m, 53.23GB read
        Requests/sec: 1574468.85
        Transfer/sec:      0.89GB
    ```

+ wrk -t64 -c 10000 -d 60s

    + CPU_NUM=1

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    29.21ms   19.63ms   2.00s    96.78%
            Req/Sec     3.23k     2.56k   33.73k    69.48%
          3411275 requests in 1.00m, 1.92GB read
          Socket errors: connect 0, read 0, write 17, timeout 2195
        Requests/sec:  56759.44
        Transfer/sec:     32.69MB
    ```

    + CPU_NUM=4

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    32.18ms   36.56ms   2.00s    99.24%
            Req/Sec     4.15k     3.04k   67.67k    85.34%
          13016579 requests in 1.00m, 7.32GB read
          Socket errors: connect 0, read 0, write 0, timeout 7993
        Requests/sec: 216584.83
        Transfer/sec:    124.76MB
    ```

    + CPU_NUM=10

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    21.58ms   63.22ms   2.00s    99.26%
            Req/Sec     8.95k     5.36k  176.68k    96.01%
          32173454 requests in 1.00m, 18.10GB read
          Socket errors: connect 0, read 0, write 412, timeout 2833
        Requests/sec: 535334.23
        Transfer/sec:    308.36MB
    ```

    + CPU_NUM=20

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    57.41ms  114.54ms   1.55s    88.57%
            Req/Sec    14.27k     8.64k  207.24k    93.97%
          51933757 requests in 1.00m, 29.21GB read
        Requests/sec: 864130.80
        Transfer/sec:    497.76MB
    ```

    + CPU_NUM=40

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   107.45ms  190.12ms   1.92s    86.64%
            Req/Sec    21.21k    17.71k  217.06k    82.85%
          77166752 requests in 1.00m, 43.41GB read
          Socket errors: connect 0, read 1, write 448, timeout 710
        Requests/sec: 1284070.69
        Transfer/sec:    739.65MB
    ```

MIO Result
----------

+ wrk -t64 -c 100 -d 60s

    + +RTS -N1

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   408.04us  332.97us  31.88ms   99.96%
            Req/Sec     2.49k    41.85     2.76k    88.86%
          9523021 requests in 1.00m, 5.36GB read
        Requests/sec: 158453.49
        Transfer/sec:     91.27MB
    ```

    + +RTS -N4

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   173.72us    1.19ms 203.51ms   99.96%
            Req/Sec     6.21k   531.89     8.40k    65.27%
          23776815 requests in 1.00m, 13.37GB read
        Requests/sec: 395625.69
        Transfer/sec:    227.89MB
    ```

    + +RTS -N10

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   155.37us    1.11ms 203.15ms   99.90%
            Req/Sec     7.34k   604.08     9.47k    74.15%
          28096444 requests in 1.00m, 15.80GB read
        Requests/sec: 467499.10
        Transfer/sec:    269.29MB
    ```

    + +RTS -N20

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   216.87us    1.17ms 202.24ms   98.00%
            Req/Sec     7.78k   709.35     9.70k    65.49%
          29752350 requests in 1.00m, 16.74GB read
        Requests/sec: 495051.01
        Transfer/sec:    285.16MB
    ```
        
    + +RTS -N40

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   789.20us    2.93ms 202.93ms   93.34%
            Req/Sec     6.88k     0.88k   10.33k    70.45%
          26310177 requests in 1.00m, 14.80GB read
        Requests/sec: 437774.02
        Transfer/sec:    252.17MB
    ```

+ wrk -t64 -c 1000 -d 60s

    + +RTS -N1 -H128m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     7.67ms   11.10ms 624.20ms   99.75%
            Req/Sec     2.07k   584.19    12.64k    97.26%
          7688338 requests in 1.00m, 4.32GB read
        Requests/sec: 127925.75
        Transfer/sec:     73.69MB
    ```

    + +RTS -N4 -H128m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     2.17ms    7.13ms 418.23ms   99.82%
            Req/Sec     7.77k     2.33k   58.37k    97.09%
          28912629 requests in 1.00m, 16.26GB read
        Requests/sec: 481076.21
        Transfer/sec:    277.11MB
    ```

    + +RTS -N10 -H128m

    ```
         Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     1.38ms   10.80ms 408.56ms   99.61%
            Req/Sec    19.72k     9.85k   89.07k    74.86%
          73745834 requests in 1.00m, 41.48GB read
        Requests/sec: 1227062.89
        Transfer/sec:    706.81MB
    ```
        
    + +RTS -N20 -H128m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     0.92ms    6.96ms 445.24ms   99.85%
            Req/Sec    25.24k    17.47k   86.68k    69.00%
          95302544 requests in 1.00m, 53.61GB read
        Requests/sec: 1585743.53
        Transfer/sec:      0.89GB
    ```

    + +RTS -N40 -H128m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     1.45ms    8.10ms 604.40ms   98.78%
            Req/Sec    23.63k    16.70k   87.41k    73.15%
          89082204 requests in 1.00m, 50.11GB read
        Requests/sec: 1482265.37
        Transfer/sec:    853.81MB
    ```

+ wrk -t64 -c 5000 -d 60s

    + +RTS -N1 -H512m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    46.33ms   53.64ms   1.83s    99.34%
            Req/Sec     1.50k     1.70k   57.32k    96.92%
          5302210 requests in 1.00m, 2.98GB read
          Socket errors: connect 0, read 1, write 869, timeout 808
        Requests/sec:  88224.71
        Transfer/sec:     50.82MB
    ```

    + +RTS -N4 -H512m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    15.35ms   53.57ms   1.64s    99.25%
            Req/Sec     6.43k     6.22k  215.91k    96.85%
          22582328 requests in 1.00m, 12.70GB read
          Socket errors: connect 0, read 0, write 1300, timeout 269
        Requests/sec: 375744.75
        Transfer/sec:    216.44MB
    ```

    + +RTS -N10 -H512m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     9.03ms   53.38ms   1.62s    99.20%
            Req/Sec    14.56k     8.18k  215.74k    95.88%
          53301081 requests in 1.00m, 29.98GB read
          Socket errors: connect 0, read 3, write 382, timeout 474
        Requests/sec: 886884.28
        Transfer/sec:    510.86MB
    ```
        
    + +RTS -N20 -H512m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    14.76ms   54.48ms   2.00s    93.79%
            Req/Sec    22.50k    13.66k  222.36k    87.02%
          82295765 requests in 1.00m, 46.29GB read
          Socket errors: connect 0, read 0, write 286, timeout 111
        Requests/sec: 1369313.42
        Transfer/sec:    788.75MB
    ```

    + +RTS -N40 -H512m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
          64 threads and 5000 connections
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    30.14ms   73.83ms   1.34s    88.31%
            Req/Sec    26.80k    18.47k  221.34k    84.55%
          100106732 requests in 1.00m, 56.31GB read
        Requests/sec: 1665674.49
        Transfer/sec:      0.94GB
    ```

+ wrk -t64 -c 10000 -d 60s

    ```
    + +RTS -N1 -H1G

          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    74.52ms   71.07ms   1.89s    96.52%
            Req/Sec     1.43k     1.64k   58.16k    93.41%
          5137402 requests in 1.00m, 2.89GB read
          Socket errors: connect 0, read 0, write 9804, timeout 1320
        Requests/sec:  85481.18
        Transfer/sec:     49.24MB
    ```

    + +RTS -N4 -H1G

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    23.52ms   53.97ms   1.82s    99.28%
            Req/Sec     5.95k     5.08k  200.45k    92.28%
          21438863 requests in 1.00m, 12.06GB read
          Socket errors: connect 0, read 2, write 7125, timeout 1145
        Requests/sec: 356721.83
        Transfer/sec:    205.48MB
    ```

    + +RTS -N10 -H1G

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    36.51ms   97.73ms   1.93s    91.06%
            Req/Sec    14.33k    11.88k  240.42k    92.61%
          51852356 requests in 1.00m, 29.17GB read
          Socket errors: connect 0, read 1, write 4937, timeout 1090
        Requests/sec: 862757.91
        Transfer/sec:    496.97MB
    ```

    + +RTS -N20 -H1G

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    98.05ms  176.43ms   1.99s    87.34%
            Req/Sec    22.19k    18.53k  219.20k    81.63%
          81609457 requests in 1.00m, 45.91GB read
          Socket errors: connect 0, read 0, write 1681, timeout 1050
        Requests/sec: 1357897.69
        Transfer/sec:    782.18MB
    ```

    + +RTS -N40 -H1G
        
    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   100.20ms  185.25ms   1.95s    87.44%
            Req/Sec    25.89k    20.32k  241.03k    79.00%
          92309475 requests in 1.00m, 51.93GB read
          Socket errors: connect 0, read 0, write 2245, timeout 528
        Requests/sec: 1535947.90
        Transfer/sec:      0.86GB
    ```

stdio Result
-----------

+ wrk -t64 -c 100 -d 60s

    + +RTS -N1

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   408.39us  275.00us  28.07ms   99.91%
            Req/Sec     2.48k    55.54     2.82k    78.71%
          9489494 requests in 1.00m, 5.34GB read
        Requests/sec: 157897.47
        Transfer/sec:     90.95MB
    ```

    + +RTS -N4

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   163.44us  576.24us  50.48ms   99.94%
            Req/Sec     6.54k   561.23     8.46k    72.05%
          25044551 requests in 1.00m, 14.09GB read
        Requests/sec: 416717.21
        Transfer/sec:    240.04MB
    ```

    + +RTS -N10

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   138.14us  270.68us  32.60ms   99.84%
            Req/Sec     7.47k   595.26     9.36k    71.71%
          28577603 requests in 1.00m, 16.08GB read
        Requests/sec: 475501.02
        Transfer/sec:    273.90MB
    ```

    + +RTS -N20

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   206.20us  732.93us  39.21ms   97.99%
            Req/Sec     7.81k   698.83     9.82k    65.79%
          29867235 requests in 1.00m, 16.80GB read
        Requests/sec: 496961.98
        Transfer/sec:    286.26MB
    ```

    + +RTS -N40

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   694.34us    2.47ms  56.92ms   93.54%
            Req/Sec     7.09k   810.58    10.67k    72.64%
          27110698 requests in 1.00m, 15.25GB read
        Requests/sec: 451091.16
        Transfer/sec:    259.84MB
    ```
                
+ wrk -t64 -c 1000 -d 60s

    + +RTS -N1 -H128m
        
    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     7.06ms    8.42ms 417.09ms   99.77%
            Req/Sec     2.23k   531.03    11.96k    97.97%
          8350230 requests in 1.00m, 4.70GB read
        Requests/sec: 139100.52
        Transfer/sec:     80.12MB
    ```

    + +RTS -N4 -H128m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     2.15ms    8.52ms 600.97ms   99.82%
            Req/Sec     8.06k     2.33k   58.77k    97.39%
          30064523 requests in 1.00m, 16.91GB read
        Requests/sec: 500244.13
        Transfer/sec:    288.15MB
    ```

    + +RTS -N10 -H128m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     1.10ms    6.96ms 407.17ms   99.83%
            Req/Sec    20.08k    11.59k   97.25k    82.69%
          75680367 requests in 1.00m, 42.57GB read
        Requests/sec: 1259246.10
        Transfer/sec:    725.35MB
    ```

    + +RTS -N20 -H128m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     0.90ms    6.07ms 402.15ms   99.64%
            Req/Sec    26.57k    16.75k   82.49k    70.18%
          99498940 requests in 1.00m, 55.97GB read
        Requests/sec: 1655556.96
        Transfer/sec:      0.93GB
    ```

    + +RTS -N40 -H128m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     0.87ms    3.36ms 204.32ms   98.19%
            Req/Sec    26.94k    19.11k   83.54k    69.64%
          101874297 requests in 1.00m, 57.31GB read
        Requests/sec: 1695086.16
        Transfer/sec:      0.95GB
    ```

+ wrk -t64 -c 5000 -d 60s

    + +RTS -N1 -H512m
        
    ```
        Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    57.89ms   64.78ms   1.83s    99.18%
            Req/Sec     1.43k     1.01k   39.80k    95.64%
          5213208 requests in 1.00m, 2.93GB read
          Socket errors: connect 0, read 0, write 253, timeout 62
        Requests/sec:  86742.38
        Transfer/sec:     49.97MB
    ```

    + +RTS -N4 -H512m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    11.53ms   23.94ms   1.03s    99.58%
            Req/Sec     7.41k     6.52k  219.35k    95.86%
          24928721 requests in 1.00m, 14.02GB read
          Socket errors: connect 0, read 0, write 1765, timeout 0
        Requests/sec: 414789.98
        Transfer/sec:    238.93MB
    ```

    + +RTS -N10 -H512m

    ```
        Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency     7.48ms   34.56ms   1.89s    98.55%
            Req/Sec    16.64k    11.93k  216.09k    92.56%
          58279865 requests in 1.00m, 32.78GB read
          Socket errors: connect 0, read 0, write 1354, timeout 2
        Requests/sec: 969734.87
        Transfer/sec:    558.59MB
    ```

    + +RTS -N20 -H512m

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    41.07ms   78.58ms   1.95s    84.98%
            Req/Sec    25.19k    14.98k  200.35k    74.38%
          95447110 requests in 1.00m, 53.69GB read
          Socket errors: connect 0, read 0, write 0, timeout 11
        Requests/sec: 1588174.55
        Transfer/sec:      0.89GB
    ```

    + +RTS -N40 -H512m

    ```
         Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    36.39ms   72.56ms   1.90s    85.04%
            Req/Sec    29.34k    18.90k  248.16k    82.50%
          104335164 requests in 1.00m, 58.69GB read
          Socket errors: connect 0, read 0, write 4, timeout 0
        Requests/sec: 1736065.48
        Transfer/sec:      0.98GB
    ```

+ wrk -t64 -c 10000 -d 60s

    + +RTS -N1 -H1G

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    99.18ms   74.00ms   1.93s    91.26%
            Req/Sec     1.46k     1.40k   69.09k    93.07%
          4846298 requests in 1.00m, 2.73GB read
          Socket errors: connect 0, read 1, write 7405, timeout 84
        Requests/sec:  80636.95
        Transfer/sec:     46.45MB
    ```

    + +RTS -N4 -H1G

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    32.64ms   56.77ms   1.72s    99.24%
            Req/Sec     5.28k     5.50k  215.68k    98.65%
          19100831 requests in 1.00m, 10.74GB read
          Socket errors: connect 0, read 0, write 1028, timeout 364
        Requests/sec: 317822.37
        Transfer/sec:    183.07MB
    ```

    + +RTS -N10 -H1G

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    70.78ms  138.64ms   1.92s    89.67%
            Req/Sec    16.53k    11.80k  213.44k    90.95%
          56451655 requests in 1.00m, 31.76GB read
          Socket errors: connect 0, read 1, write 3304, timeout 4
        Requests/sec: 939321.78
        Transfer/sec:    541.07MB
    ```
        
    + +RTS -N20 -H1G

    ```
          Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency    98.42ms  177.15ms   1.94s    87.26%
            Req/Sec    22.73k    18.83k  224.83k    80.63%
          81726967 requests in 1.00m, 45.97GB read
          Socket errors: connect 0, read 0, write 2055, timeout 223
        Requests/sec: 1359850.47
        Transfer/sec:    783.30MB
    ```

    + +RTS -N40 -H1G

    ```
         Thread Stats   Avg      Stdev     Max   +/- Stdev
            Latency   104.18ms  189.55ms   1.95s    86.95%
            Req/Sec    26.12k    21.81k  213.89k    81.17%
          92825020 requests in 1.00m, 52.22GB read
          Socket errors: connect 0, read 1, write 1640, timeout 370
        Requests/sec: 1544509.66
        Transfer/sec:      0.87GB
    ```
