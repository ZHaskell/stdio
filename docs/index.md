Design overview
===============

This video is recorded @shenzhen chinese haskell meeting by @winterland1989, it explains almost every detail how do we combine GHC's lightweight thread with libuv's event loop in haskell-stdio.

[![libuv based I/O manager](https://img.youtube.com/vi/2J0fGMpFA_w/0.jpg)](https://youtu.be/2J0fGMpFA_w) 

Haskell-stdio combine GHC's lightweight thread with libuv's event loop, it use one I/O manager thread per capability, blow is the thread structures(style borrowed from [ghc-illustrated](https://github.com/takenobu-hs/haskell-ghc-illustrated)).

![haskell-stdio thread structures](./docs/io-manager-thread-structure.png)

And the program control flow:

![haskell-stdio control flow](./docs/io-manager-flow.png)
