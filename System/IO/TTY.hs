{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|
Module      : System.IO.TTY
Description : TCP or IPC servers and clients
Copyright   : (c) Winterland, 2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provides an API for opening tty as 'UVStream'. In most case, it will not be necessary to use this module directly

-}

module System.IO.TTY(
    UVStream
  , stdin
  , stdout
  , stderr
  ) where

import System.IO.UV.Manager
import System.IO.Exception
import System.IO.Unsafe
import System.IO.Resource

stdin :: UVStream
{-# NOINLINE stdin #-}
stdin = unsafePerformIO $ do
    uvm <- getUVManager
    (stdin, _ ) <- acquire (initTTYStream 0 uvm)    -- well, stdin live across whole program
    return stdin                                    -- so we give up resource management

stdout :: UVStream
{-# NOINLINE stdout #-}
stdout = unsafePerformIO $ do
    uvm <- getUVManager
    (stdin, _ ) <- acquire (initTTYStream 1 uvm)
    return stdin

stderr :: UVStream
{-# NOINLINE stderr #-}
stderr = unsafePerformIO $ do
    uvm <- getUVManager
    (stdin, _ ) <- acquire (initTTYStream 2 uvm)
    return stdin
