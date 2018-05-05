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

import System.IO.UV.Stream
import System.IO.Exception
import System.IO.Unsafe

stdin :: UVStream
{-# NOINLINE stdin #-}
stdin = unsafePerformIO $ do
    (stdin, _ ) <- acquire (initTTYStream 0)    -- well, stdin live across whole program
    return stdin                                -- so we give up resource management

stdout :: UVStream
{-# NOINLINE stdout #-}
stdout = unsafePerformIO $ do
    (stdin, _ ) <- acquire (initTTYStream 1)
    return stdin

stderr :: UVStream
{-# NOINLINE stderr #-}
stderr = unsafePerformIO $ do
    (stdin, _ ) <- acquire (initTTYStream 2)
    return stdin
