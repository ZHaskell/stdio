Haskell stdio: haskell standard input and output
================================================

[![Linux Build Status](https://img.shields.io/travis/haskell-stdio/stdio/master.svg?label=Linux%20build)](https://travis-ci.org/haskell-stdio/stdio)
[![Windows Build Status](https://img.shields.io/appveyor/ci/winterland1989/stdio-7usux/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/winterland1989/stdio-7usux/branch/master)

Welcome! Haskell stdio is a complete I/O toolkit powered by libuv, it features a multi-core io multiplexer and various improvements on packed data types. This project is still in infancy. Please join in!

```
    __  _____   _____ __ __ ________    __       _______________  ________ 
   / / / /   | / ___// //_// ____/ /   / /      / ___/_  __/ __ \/  _/ __ \
  / /_/ / /| | \__ \/ ,<  / __/ / /   / /       \__ \ / / / / / // // / / /
 / __  / ___ |___/ / /| |/ /___/ /___/ /___    ___/ // / / /_/ // // /_/ / 
/_/ /_/_/  |_/____/_/ |_/_____/_____/_____/   /____//_/ /_____/___/\____/
```

Install
-------

On windows we have bundled libuv source, so no extra steps to be taken.

On \*nix platforms, you should install libuv library first, you can use your distribution's package manager if available, for example:

```
# on debian/ubuntu, make sure to use 1.x
apt-get install libuv1-dev  libuv1

# on MacOS, we recommend brew
brew install libuv

...
```

Currently **the minimum version requirement for libuv is v1.14**. If your package manager's libuv doesn't meet this requirement, you can also build libuv from source following the guide [here](https://github.com/libuv/libuv#build-instructions), e.g.

```
git clone https://github.com/libuv/libuv.git 
cd libuv 
git checkout tags/v1.24.0   # depend on your own need, any version >= 1.14 will work.
sh autogen.sh 
./configure 
make 
sudo make install 
```

After manually building and installing, you may need to modify your `LIBRARY_PATH/CPATH` if necessary. Now installing stdio is as easy as any other haskell packages.  

```
cabal install stdio
```

Now you can fire GHCi and play around, or read the [project overview](https://haskell-stdio.github.io/stdio), [haddock](http://hackage.haskell.org/package/stdio).

Examples
--------

+ hello world

```
import Std.IO.StdStream
import qualified Std.Data.Text as T

main = do
    -- read stdin and write to stdout, but with our new IO manager!
    input <- readLineStd
    printStd (T.validate input)
```

+ tcp echo server

```
import Std.IO.TCP
import Std.IO.Buffered
import Control.Monad

main = do
    startServer defaultServerConfig
        { serverAddr = SockAddrInet 8888 inetAny
        , serverWorker = echo
        }
  where
    echo uvs = forever $ do
        i <- newBufferedInput uvs 4096
        o <- newBufferedOutput uvs 4096
        readBuffer i >>= writeBuffer o
        flushBuffer o
```

Now try `nc -v 127.0.0.1 8888`.

+ logging

```
import Std.IO.Logger
import qualified Std.Data.Builder as B
import Control.Concurrent

main = withStdLogger $ do
    debug $ "hello world! PI ~=" >> B.double pi     -- debug level won't be immediately flushed
    forkIO $ do
        fatal "fatal message will trigger a log flush"
```

+ file system operatations

```
import           Std.IO.FileSystem
import           Std.IO.Resource
import           Std.IO.StdStream

main = do
    -- create a temp directory
    tempdir <- mkdtemp "temp"   
    let filename = "temp" <> "/test"
        flags = O_RDWR .|. O_CREAT      -- create if not exist
        mode = DEFAULT_MODE

    -- file is a 'Resource', use 'withResource' to automatically manage it
    withResource (initUVFile filename flags mode) $ \ f -> do
        o <- newBufferedOutput file 4096
        writeBuffer o "hello world!"
        flushBuffer o

    stat filename >>= printStd
```
