Haskell stdio: haskell standard input and output
================================================

[![Linux Build Status](https://img.shields.io/travis/winterland1989/stdio/master.svg?label=Linux%20build)](https://travis-ci.org/winterland1989/stdio)
[![Windows Build Status](https://img.shields.io/appveyor/ci/winterland1989/stdio/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/winterland1989/stdio/branch/master)

Welcome! Haskell stdio is an complete I/O toolkit powered by libuv, it features a multiple core io multiplexer and various improvements on packed data types.

![stdio banner](./img/banner.png)

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

Now you can fire GHCi and play around, or read the [design overview](), [haddock]() or [examples]().

Contribution Guide
------------------

This project welcomes new contributors, and use github issue/pull-request workflow. If you ever find new bugs/ideas/designs, please shout out loud.
