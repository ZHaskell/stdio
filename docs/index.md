Design overview
===============

haskell-stdio is a self contained IO toolkit for Haskell (more specifically, GHC), modules can be divided into three categories:

1. Packed data structures:

    + Unified `Array` interface, for many array types GHC RTS provide.

        - [Std.Data.Array](http://hackage.haskell.org/package/stdio/docs/Std-Data-Array.html) Unified `Array` interface, `Arr` class and many instances.
        - [Std.Data.Array.Checked](http://hackage.haskell.org/package/stdio/docs/Std-Data-Array-Checked.html) Bounded checked version, with exactly same API with module above.

    + `Vector` (array slices) operatation, including `Bytes`(word8 vectors).

        - [Std.Data.Vector](http://hackage.haskell.org/package/stdio/docs/Std-Data-Vector.html) `Vector` umbrella module, re-exports many things from following modules.
        - [Std.Data.Vector.Base](http://hackage.haskell.org/package/stdio/docs/Std-Data-Vector-Base.html) `Vec` class, `Vector` and `PrimVector` type, basic operations and instances.
        - [Std.Data.Vector.Extra](http://hackage.haskell.org/package/stdio/docs/Std-Data-Vector-Extra.html) Various slice manipulations, loopings and foldings.
        - [Std.Data.Vector.Search](http://hackage.haskell.org/package/stdio/docs/Std-Data-Vector-Search.html) KMP search on vectors!
        - [Std.Data.Vector.Sort](http://hackage.haskell.org/package/stdio/docs/Std-Data-Vector-Sort.html) Merge & radix sort for vectors.
        - [Std.Data.Vector.QQ](http://hackage.haskell.org/package/stdio/docs/Std-Data-Vector-QQ.html) Numeric vector literals.

    + `Builder` and `Parser` type for encoding/decoding between haskell values and `Bytes`.

        - [Std.Data.Builder](http://hackage.haskell.org/package/stdio/docs/Std-Data-Builder.html) `Builder` umbrella module, re-exports many things from following modules.
        - [Std.Data.Builder.Base](http://hackage.haskell.org/package/stdio/docs/Std-Data-Builder-Base.html) `Builder` monad type, basic builders.
        - [Std.Data.Builder.Numeric](http://hackage.haskell.org/package/stdio/docs/Std-Data-Builder-Numeric.html) Various numeric builders, including fast IEEE float builders! 
    
        - [Std.Data.Parser](http://hackage.haskell.org/package/stdio/docs/Std-Data-Parser.html) `Parser` umbrella module, re-exports many things from following modules.
        - [Std.Data.Parser.Base](http://hackage.haskell.org/package/stdio/docs/Std-Data-Parser-Base.html) `Parser` monad type, basic parsers.
        - [Std.Data.Builder.Numeric](http://hackage.haskell.org/package/stdio/docs/Std-Data-Builder-Numeric.html) Various numeric parsers.
    
2. IO related stuff:

    + `Buffered` IO support, for reading and writing `Bytes`.
    
        - [Std.IO.Buffered](http://hackage.haskell.org/package/stdio/docs/Std-IO-Buffered.html) `Input` and `Output` device, `BufferedInput` and `BufferedOutput` operations.
        
    + Standard input and output stream.
        - [Std.IO.StdStream](http://hackage.haskell.org/package/stdio/docs/Std-IO-StdStream.html) `StdStream` type, and `stdin`, `stdout`, etc. 

    + `TCP` socket support.
        - [Std.IO.TCP](http://hackage.haskell.org/package/stdio/docs/Std-IO-TCP.html) Fast TCP client and server.

    + `FileSystem` support, with special path type `CBytes`.
        - [Std.Data.CBytes](http://hackage.haskell.org/package/stdio/docs/Std-Data-TCP.CBytes) Null terminated `CBytes`, suitable for file path, system FFI, etc. 
        - [Std.IO.FileSystem](http://hackage.haskell.org/package/stdio/docs/Std-IO-FileSystem.html) File system operations implemented with unsafe FFI.
        - [Std.IO.FileSystemT](http://hackage.haskell.org/package/stdio/docs/Std-IO-FileSystem.html) File system operations implemented with libuv's threadpool.

    + Simple yet fast `Logger`.
        - [Std.IO.Logger](http://hackage.haskell.org/package/stdio/docs/Std-IO-FileSystem.html) Simple fast and thread safe logger.

    + High performance low resolution timers.
        - [Std.IO.LowResTimer](http://hackage.haskell.org/package/stdio/docs/Std-IO-LowResTimer.html) Low resolutioin (0.1s) timer, `timeout`, `throttle`, etc. 

    + Resource management and exceptions.
        - [Std.IO.Resource](http://hackage.haskell.org/package/stdio/docs/Std-IO-Resource.html)  `Resource` monad for safely using a resource.
        - [Std.IO.Exception](http://hackage.haskell.org/package/stdio/docs/Std-IO-Exception.html)  `Control.Exception` replacement.
        
3. Internal modules for hackers:

    + High performance IO management built upon libuv and GHC RTS.

        - [Std.IO.UV.Manager](http://hackage.haskell.org/package/stdio/docs/Std-IO-UV-Manager.html) Our standalone IO manager based on libuv event loop.
        - [Std.IO.UV.Errno](http://hackage.haskell.org/package/stdio/docs/Std-IO-UV-Errno.html) Defination of all libuv's errno.
        - [Std.IO.UV.FFI](http://hackage.haskell.org/package/stdio/docs/Std-IO-UV-Errno.html) Foreign imports of all libuv's FFI.
        
Currently we also have plans to support more things such as JSON protocols, UDP socket, etc, which will also built upon works above. Basically we focus on engineering infrastructure only. Please join in!

Related material
----------------

We have published a [paper](https://dl.acm.org/citation.cfm?id=3242759) on our IO manager at Haskell Symposium 2018, it's available [here](https://github.com/haskell-stdio/stdio/blob/master/docs/A%20High-Performance%20Multicore%20IO%20Manager%20Based%20on%20libuv%20(Experience%20Report).pdf).

Here is a video recorded @shenzhen chinese haskell meeting by @winterland1989, it explains almost every detail how do we combine GHC's lightweight thread with libuv's event loop in haskell-stdio.

[![libuv based I/O manager](https://img.youtube.com/vi/2J0fGMpFA_w/0.jpg)](https://youtu.be/2J0fGMpFA_w) 
