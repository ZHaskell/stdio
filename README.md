Haskell stdio: haskell standard input and output
================================================

[![Linux Build Status](https://img.shields.io/travis/haskell-stdio/stdio/master.svg?label=Linux%20build)](https://travis-ci.org/haskell-stdio/stdio)
[![Windows Build Status](https://img.shields.io/appveyor/ci/winterland1989/stdio-7usux/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/winterland1989/stdio-7usux/branch/master)

Welcome! Haskell stdio is an complete I/O toolkit powered by libuv, it features a multiple core io multiplexer and various improvements on packed data types.

```
    __  _____   _____ __ __ ________    __       _______________  ________ 
   / / / /   | / ___// //_// ____/ /   / /      / ___/_  __/ __ \/  _/ __ \
  / /_/ / /| | \__ \/ ,<  / __/ / /   / /       \__ \ / / / / / // // / / /
 / __  / ___ |___/ / /| |/ /___/ /___/ /___    ___/ // / / /_/ // // /_/ / 
/_/ /_/_/  |_/____/_/ |_/_____/_____/_____/   /____//_/ /_____/___/\____/
```

User Guide
----------

On windows we have bundled libuv source, so not extra steps to be taken.

On \*nix platforms, you should install libuv library first, you can use your distribution's package manager if available, for example:

```
# on debian/ubuntu, make sure to use 1.x
apt-get install libuv1-dev  libuv1

# on MacOS, we recommend brew
brew install libuv

...
```

You can also build libuv from source following the guide [here](https://github.com/libuv/libuv#build-instructions), and modify your `LIBRARY_PATH/CPATH` if necessary. After libuv is in place, installing stdio is as easy as any other haskell packages.

```
cabal install stdio
```

Now you can fire GHCi and play around, or read the [design overview](https://haskell-stdio.github.io/stdio), [haddock](https://haskell-stdio.github.io/stdio/haddock/) or [examples]().

Contribution Guide
------------------

This project welcomes new contributors, and use github issue/pull-request workflow. If you ever find new bugs/ideas/designs, please shout out loud.
