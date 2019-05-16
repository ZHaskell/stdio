JSON benchmarks
===============

Here is a benchmark comparing parsing step, i.e. parsing JSON doc into Haskell JSON value type. `stdio` does not provide lazy parsing, while `aeson` provides lazy variation which skip `HashMap` and `Vector` constructing. Since `stdio` use key-value vector to represent JSON object, it's useful to compare with both strict and lazy variations.

Overall `stdio` is 2~3x than aeson in parsing.

```
» python ./bench-aeson.py                                                                                                           donghan@bogon
    json-data/twitter1.json :: 60000 times
      1.246 seconds, 48148 parses/sec, 39.260 MB/sec
    json-data/twitter1.json :: 60000 times
      1.288 seconds, 46569 parses/sec, 37.973 MB/sec
    json-data/twitter1.json :: 60000 times
      1.296 seconds, 46307 parses/sec, 37.758 MB/sec
0.8 KB: 48148 msg\/sec (39.3 MB\/sec)
    json-data/twitter10.json :: 13000 times
      1.327 seconds, 9796 parses/sec, 61.605 MB/sec
    json-data/twitter10.json :: 13000 times
      1.303 seconds, 9978 parses/sec, 62.749 MB/sec
    json-data/twitter10.json :: 13000 times
      1.312 seconds, 9909 parses/sec, 62.315 MB/sec
6.4 KB: 9978 msg\/sec (62.7 MB\/sec)
    json-data/twitter20.json :: 7500 times
      1.529 seconds, 4905 parses/sec, 56.452 MB/sec
    json-data/twitter20.json :: 7500 times
      1.520 seconds, 4935 parses/sec, 56.803 MB/sec
    json-data/twitter20.json :: 7500 times
      1.518 seconds, 4941 parses/sec, 56.867 MB/sec
11.8 KB: 4941 msg\/sec (56.9 MB\/sec)
    json-data/twitter50.json :: 2500 times
      1.557 seconds, 1605 parses/sec, 48.924 MB/sec
    json-data/twitter50.json :: 2500 times
      1.433 seconds, 1744 parses/sec, 53.151 MB/sec
    json-data/twitter50.json :: 2500 times
      1.413 seconds, 1769 parses/sec, 53.922 MB/sec
31.2 KB: 1770 msg\/sec (53.9 MB\/sec)
    json-data/twitter100.json :: 1000 times
      1.237 seconds, 808 parses/sec, 48.555 MB/sec
    json-data/twitter100.json :: 1000 times
      1.210 seconds, 826 parses/sec, 49.671 MB/sec
    json-data/twitter100.json :: 1000 times
      1.223 seconds, 817 parses/sec, 49.142 MB/sec
61.5 KB: 827 msg\/sec (49.7 MB\/sec)
    json-data/jp10.json :: 4000 times
      1.150 seconds, 3477 parses/sec, 49.694 MB/sec
    json-data/jp10.json :: 4000 times
      0.979 seconds, 4086 parses/sec, 58.385 MB/sec
    json-data/jp10.json :: 4000 times
      0.987 seconds, 4053 parses/sec, 57.918 MB/sec
14.6 KB: 4086 msg\/sec (58.4 MB\/sec)
    json-data/jp50.json :: 1200 times
      1.020 seconds, 1176 parses/sec, 50.662 MB/sec
    json-data/jp50.json :: 1200 times
      1.020 seconds, 1176 parses/sec, 50.635 MB/sec
    json-data/jp50.json :: 1200 times
      1.026 seconds, 1169 parses/sec, 50.367 MB/sec
44.1 KB: 1177 msg\/sec (50.7 MB\/sec)
    json-data/jp100.json :: 700 times
      1.375 seconds, 509 parses/sec, 41.210 MB/sec
    json-data/jp100.json :: 700 times
      1.212 seconds, 577 parses/sec, 46.754 MB/sec
    json-data/jp100.json :: 700 times
      1.207 seconds, 579 parses/sec, 46.926 MB/sec
82.9 KB: 580 msg\/sec (46.9 MB\/sec)
------------------------------------------------------------
» python ./bench-aeson-lazy.py                                                                                                      donghan@bogon
    json-data/twitter1.json :: 60000 times
      0.988 seconds, 60730 parses/sec, 49.519 MB/sec
    json-data/twitter1.json :: 60000 times
      1.000 seconds, 60022 parses/sec, 48.942 MB/sec
    json-data/twitter1.json :: 60000 times
      0.970 seconds, 61830 parses/sec, 50.416 MB/sec
0.8 KB: 61831 msg\/sec (50.4 MB\/sec)
    json-data/twitter10.json :: 13000 times
      0.927 seconds, 14020 parses/sec, 88.170 MB/sec
    json-data/twitter10.json :: 13000 times
      0.931 seconds, 13965 parses/sec, 87.821 MB/sec
    json-data/twitter10.json :: 13000 times
      0.913 seconds, 14244 parses/sec, 89.575 MB/sec
6.4 KB: 14244 msg\/sec (89.6 MB\/sec)
    json-data/twitter20.json :: 7500 times
      1.016 seconds, 7385 parses/sec, 84.996 MB/sec
    json-data/twitter20.json :: 7500 times
      1.023 seconds, 7330 parses/sec, 84.366 MB/sec
    json-data/twitter20.json :: 7500 times
      1.019 seconds, 7360 parses/sec, 84.713 MB/sec
11.8 KB: 7385 msg\/sec (85.0 MB\/sec)
    json-data/twitter50.json :: 2500 times
      0.962 seconds, 2598 parses/sec, 79.174 MB/sec
    json-data/twitter50.json :: 2500 times
      0.938 seconds, 2666 parses/sec, 81.248 MB/sec
    json-data/twitter50.json :: 2500 times
      0.943 seconds, 2650 parses/sec, 80.768 MB/sec
31.2 KB: 2667 msg\/sec (81.2 MB\/sec)
    json-data/twitter100.json :: 1000 times
      0.891 seconds, 1122 parses/sec, 67.419 MB/sec
    json-data/twitter100.json :: 1000 times
      0.901 seconds, 1109 parses/sec, 66.653 MB/sec
    json-data/twitter100.json :: 1000 times
      0.893 seconds, 1119 parses/sec, 67.271 MB/sec
61.5 KB: 1122 msg\/sec (67.4 MB\/sec)
    json-data/jp10.json :: 4000 times
      0.787 seconds, 5084 parses/sec, 72.654 MB/sec
    json-data/jp10.json :: 4000 times
      0.787 seconds, 5082 parses/sec, 72.619 MB/sec
    json-data/jp10.json :: 4000 times
      0.786 seconds, 5085 parses/sec, 72.672 MB/sec
14.6 KB: 5086 msg\/sec (72.7 MB\/sec)
    json-data/jp50.json :: 1200 times
      0.795 seconds, 1508 parses/sec, 64.947 MB/sec
    json-data/jp50.json :: 1200 times
      0.803 seconds, 1494 parses/sec, 64.330 MB/sec
    json-data/jp50.json :: 1200 times
      0.798 seconds, 1504 parses/sec, 64.775 MB/sec
44.1 KB: 1509 msg\/sec (64.9 MB\/sec)
    json-data/jp100.json :: 700 times
      0.935 seconds, 749 parses/sec, 60.633 MB/sec
    json-data/jp100.json :: 700 times
      0.937 seconds, 747 parses/sec, 60.496 MB/sec
    json-data/jp100.json :: 700 times
      0.941 seconds, 744 parses/sec, 60.238 MB/sec
82.9 KB: 749 msg\/sec (60.6 MB\/sec)
------------------------------------------------------------
» python ./bench-stdio.py                                                                                                           donghan@bogon
    json-data/twitter1.json :: 60000 times
      0.545 seconds, 110184 parses/sec, 89.844 MB/sec
    json-data/twitter1.json :: 60000 times
      0.529 seconds, 113464 parses/sec, 92.518 MB/sec
    json-data/twitter1.json :: 60000 times
      0.533 seconds, 112552 parses/sec, 91.775 MB/sec
0.8 KB: 113464 msg\/sec (92.5 MB\/sec)
    json-data/twitter10.json :: 13000 times
      0.578 seconds, 22482 parses/sec, 141.381 MB/sec
    json-data/twitter10.json :: 13000 times
      0.577 seconds, 22545 parses/sec, 141.775 MB/sec
    json-data/twitter10.json :: 13000 times
      0.584 seconds, 22276 parses/sec, 140.089 MB/sec
6.4 KB: 22545 msg\/sec (141.8 MB\/sec)
    json-data/twitter20.json :: 7500 times
      0.659 seconds, 11382 parses/sec, 131.003 MB/sec
    json-data/twitter20.json :: 7500 times
      0.658 seconds, 11391 parses/sec, 131.104 MB/sec
    json-data/twitter20.json :: 7500 times
      0.674 seconds, 11132 parses/sec, 128.127 MB/sec
11.8 KB: 11391 msg\/sec (131.1 MB\/sec)
    json-data/twitter50.json :: 2500 times
      0.588 seconds, 4248 parses/sec, 129.460 MB/sec
    json-data/twitter50.json :: 2500 times
      0.593 seconds, 4215 parses/sec, 128.448 MB/sec
    json-data/twitter50.json :: 2500 times
      0.591 seconds, 4227 parses/sec, 128.810 MB/sec
31.2 KB: 4249 msg\/sec (129.5 MB\/sec)
    json-data/twitter100.json :: 1000 times
      0.517 seconds, 1933 parses/sec, 116.148 MB/sec
    json-data/twitter100.json :: 1000 times
      0.517 seconds, 1934 parses/sec, 116.199 MB/sec
    json-data/twitter100.json :: 1000 times
      0.518 seconds, 1932 parses/sec, 116.079 MB/sec
61.5 KB: 1934 msg\/sec (116.2 MB\/sec)
    json-data/jp10.json :: 4000 times
      0.358 seconds, 11164 parses/sec, 159.533 MB/sec
    json-data/jp10.json :: 4000 times
      0.346 seconds, 11545 parses/sec, 164.976 MB/sec
    json-data/jp10.json :: 4000 times
      0.351 seconds, 11385 parses/sec, 162.680 MB/sec
14.6 KB: 11546 msg\/sec (165.0 MB\/sec)
    json-data/jp50.json :: 1200 times
      0.359 seconds, 3346 parses/sec, 144.076 MB/sec
    json-data/jp50.json :: 1200 times
      0.353 seconds, 3397 parses/sec, 146.265 MB/sec
    json-data/jp50.json :: 1200 times
      0.365 seconds, 3283 parses/sec, 141.361 MB/sec
44.1 KB: 3398 msg\/sec (146.3 MB\/sec)
    json-data/jp100.json :: 700 times
      0.432 seconds, 1621 parses/sec, 131.243 MB/sec
    json-data/jp100.json :: 700 times
      0.439 seconds, 1593 parses/sec, 128.978 MB/sec
    json-data/jp100.json :: 700 times
      0.433 seconds, 1616 parses/sec, 130.855 MB/sec
82.9 KB: 1621 msg\/sec (131.2 MB\/sec)
```

Now we compare the speed of converting JSON values into custom ADTs (and reverse), encoding ADTs into JSON docs:

+ All instance in `stdio` case is derived via generics:
+ `stdio` use key-value vector to represent JSON object, thus it's super fast to construct.
+ For the same reason above, `stdio`'s `FromValue` instances need to construct a lookup map during converting(still very fast).

Overall `stdio` is faster than `aeson`(generic instances) in every case, and only slower than TH instances in parsing sum type, which is a comparison between TH generated `case` expression verse parser's `<|>`.

```
benchmarking D/toJSON/th
time                 3.680 μs   (3.665 μs .. 3.698 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 3.692 μs   (3.674 μs .. 3.740 μs)
std dev              88.70 ns   (43.19 ns .. 165.9 ns)
variance introduced by outliers: 28% (moderately inflated)

benchmarking D/toJSON/generic
time                 3.588 μs   (3.567 μs .. 3.612 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 3.627 μs   (3.596 μs .. 3.702 μs)
std dev              156.8 ns   (80.94 ns .. 289.8 ns)
variance introduced by outliers: 56% (severely inflated)

benchmarking D/stdio-toJSON
time                 17.37 ns   (17.33 ns .. 17.43 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 17.45 ns   (17.37 ns .. 17.57 ns)
std dev              323.5 ps   (198.1 ps .. 509.3 ps)
variance introduced by outliers: 27% (moderately inflated)

benchmarking D/encode/th
time                 4.151 μs   (4.127 μs .. 4.177 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 4.164 μs   (4.138 μs .. 4.233 μs)
std dev              120.7 ns   (57.01 ns .. 230.4 ns)
variance introduced by outliers: 36% (moderately inflated)

benchmarking D/encode/generic
time                 4.183 μs   (4.164 μs .. 4.211 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 4.193 μs   (4.178 μs .. 4.214 μs)
std dev              58.61 ns   (39.48 ns .. 92.71 ns)
variance introduced by outliers: 11% (moderately inflated)

benchmarking D/stdio-encode
time                 3.579 μs   (3.550 μs .. 3.619 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 3.567 μs   (3.549 μs .. 3.597 μs)
std dev              76.34 ns   (40.93 ns .. 119.5 ns)
variance introduced by outliers: 24% (moderately inflated)

benchmarking D/fromJSON/th
time                 1.892 μs   (1.882 μs .. 1.902 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.903 μs   (1.889 μs .. 1.962 μs)
std dev              84.53 ns   (23.50 ns .. 188.0 ns)
variance introduced by outliers: 59% (severely inflated)

benchmarking D/fromJSON/generic
time                 4.938 μs   (4.905 μs .. 4.990 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.946 μs   (4.918 μs .. 4.990 μs)
std dev              118.8 ns   (77.71 ns .. 184.0 ns)
variance introduced by outliers: 27% (moderately inflated)

benchmarking D/stdio-fromJSON
time                 2.854 μs   (2.845 μs .. 2.865 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.865 μs   (2.852 μs .. 2.883 μs)
std dev              52.00 ns   (30.06 ns .. 79.99 ns)
variance introduced by outliers: 19% (moderately inflated)

benchmarking BigRecord/toJSON/th
time                 1.971 μs   (1.962 μs .. 1.982 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.979 μs   (1.968 μs .. 1.993 μs)
std dev              42.59 ns   (32.60 ns .. 54.80 ns)
variance introduced by outliers: 25% (moderately inflated)

benchmarking BigRecord/toJSON/generic
time                 2.072 μs   (2.066 μs .. 2.081 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.081 μs   (2.074 μs .. 2.088 μs)
std dev              23.76 ns   (19.72 ns .. 30.07 ns)

benchmarking BigRecord/stdio-toJSON
time                 327.6 ns   (326.9 ns .. 328.4 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 328.8 ns   (327.7 ns .. 330.0 ns)
std dev              4.148 ns   (3.266 ns .. 5.701 ns)
variance introduced by outliers: 12% (moderately inflated)

benchmarking BigRecord/encode/th
time                 1.818 μs   (1.808 μs .. 1.829 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.817 μs   (1.811 μs .. 1.824 μs)
std dev              23.00 ns   (17.75 ns .. 32.89 ns)
variance introduced by outliers: 10% (moderately inflated)

benchmarking BigRecord/encode/generic
time                 2.062 μs   (2.050 μs .. 2.080 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.057 μs   (2.050 μs .. 2.068 μs)
std dev              28.33 ns   (18.72 ns .. 45.60 ns)
variance introduced by outliers: 12% (moderately inflated)

benchmarking BigRecord/stdio-encode
time                 1.752 μs   (1.741 μs .. 1.768 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.746 μs   (1.740 μs .. 1.756 μs)
std dev              25.20 ns   (16.92 ns .. 47.23 ns)
variance introduced by outliers: 13% (moderately inflated)

benchmarking BigRecord/fromJSON/th
time                 5.104 μs   (5.087 μs .. 5.125 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.100 μs   (5.084 μs .. 5.115 μs)
std dev              50.45 ns   (38.45 ns .. 70.93 ns)

benchmarking BigRecord/fromJSON/generic
time                 9.354 μs   (9.334 μs .. 9.378 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.356 μs   (9.329 μs .. 9.385 μs)
std dev              96.92 ns   (79.21 ns .. 121.0 ns)

benchmarking BigRecord/stdio-fromJSON
time                 6.398 μs   (6.357 μs .. 6.449 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.377 μs   (6.355 μs .. 6.401 μs)
std dev              77.60 ns   (63.38 ns .. 110.8 ns)

benchmarking BigProduct/toJSON/th
time                 375.9 ns   (370.4 ns .. 384.2 ns)
                     0.998 R²   (0.993 R² .. 1.000 R²)
mean                 370.5 ns   (367.9 ns .. 379.2 ns)
std dev              13.66 ns   (5.007 ns .. 27.49 ns)
variance introduced by outliers: 53% (severely inflated)

benchmarking BigProduct/toJSON/generic
time                 429.7 ns   (428.8 ns .. 430.6 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 430.3 ns   (429.1 ns .. 431.4 ns)
std dev              3.985 ns   (3.274 ns .. 5.086 ns)

benchmarking BigProduct/stdio-toJSON
time                 373.9 ns   (371.1 ns .. 377.1 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 373.2 ns   (371.1 ns .. 376.0 ns)
std dev              8.498 ns   (5.947 ns .. 11.42 ns)
variance introduced by outliers: 30% (moderately inflated)

benchmarking BigProduct/encode/th
time                 482.6 ns   (480.0 ns .. 486.5 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 482.5 ns   (480.8 ns .. 484.2 ns)
std dev              6.026 ns   (5.050 ns .. 7.577 ns)
variance introduced by outliers: 11% (moderately inflated)

benchmarking BigProduct/encode/generic
time                 682.0 ns   (679.1 ns .. 685.9 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 685.5 ns   (681.5 ns .. 695.5 ns)
std dev              18.79 ns   (11.29 ns .. 32.42 ns)
variance introduced by outliers: 37% (moderately inflated)

benchmarking BigProduct/stdio-encode
time                 655.4 ns   (652.1 ns .. 660.6 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 654.9 ns   (653.1 ns .. 657.9 ns)
std dev              8.152 ns   (5.531 ns .. 12.98 ns)
variance introduced by outliers: 11% (moderately inflated)

benchmarking BigProduct/fromJSON/th
time                 2.578 μs   (2.571 μs .. 2.584 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.581 μs   (2.573 μs .. 2.590 μs)
std dev              29.65 ns   (22.62 ns .. 39.51 ns)

benchmarking BigProduct/fromJSON/generic
time                 3.547 μs   (3.529 μs .. 3.574 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 3.548 μs   (3.535 μs .. 3.564 μs)
std dev              47.15 ns   (33.51 ns .. 73.34 ns)
variance introduced by outliers: 11% (moderately inflated)

benchmarking BigProduct/stdio-fromJSON
time                 2.179 μs   (2.169 μs .. 2.192 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.175 μs   (2.169 μs .. 2.182 μs)
std dev              22.26 ns   (17.42 ns .. 31.32 ns)

benchmarking BigSum/toJSON/th
time                 10.12 ns   (10.09 ns .. 10.15 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.12 ns   (10.10 ns .. 10.15 ns)
std dev              84.23 ps   (61.83 ps .. 122.0 ps)

benchmarking BigSum/toJSON/generic
time                 35.56 ns   (35.35 ns .. 35.82 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 35.48 ns   (35.37 ns .. 35.63 ns)
std dev              439.0 ps   (304.3 ps .. 640.5 ps)
variance introduced by outliers: 14% (moderately inflated)

benchmarking BigSum/stdio-toJSON
time                 14.44 ns   (14.38 ns .. 14.54 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 14.45 ns   (14.40 ns .. 14.53 ns)
std dev              209.2 ps   (151.9 ps .. 296.2 ps)
variance introduced by outliers: 19% (moderately inflated)

benchmarking BigSum/encode/th
time                 173.9 ns   (173.2 ns .. 174.6 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 173.7 ns   (173.2 ns .. 174.2 ns)
std dev              1.812 ns   (1.464 ns .. 2.329 ns)

benchmarking BigSum/encode/generic
time                 181.3 ns   (178.7 ns .. 183.7 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 179.7 ns   (178.6 ns .. 180.9 ns)
std dev              3.975 ns   (2.942 ns .. 5.163 ns)
variance introduced by outliers: 30% (moderately inflated)

benchmarking BigSum/stdio-encode
time                 53.22 ns   (52.88 ns .. 53.62 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 53.78 ns   (53.33 ns .. 54.75 ns)
std dev              2.087 ns   (1.205 ns .. 3.261 ns)
variance introduced by outliers: 60% (severely inflated)

benchmarking BigSum/fromJSON/th
time                 219.3 ns   (217.3 ns .. 221.4 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 220.9 ns   (218.7 ns .. 222.7 ns)
std dev              6.370 ns   (5.251 ns .. 7.846 ns)
variance introduced by outliers: 42% (moderately inflated)

benchmarking BigSum/fromJSON/generic
time                 1.244 μs   (1.238 μs .. 1.252 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.242 μs   (1.238 μs .. 1.247 μs)
std dev              16.39 ns   (13.70 ns .. 20.29 ns)
variance introduced by outliers: 11% (moderately inflated)

benchmarking BigSum/stdio-fromJSON
time                 908.4 ns   (903.8 ns .. 913.3 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 906.6 ns   (902.8 ns .. 911.1 ns)
std dev              13.96 ns   (10.80 ns .. 19.22 ns)
variance introduced by outliers: 16% (moderately inflated)
```
