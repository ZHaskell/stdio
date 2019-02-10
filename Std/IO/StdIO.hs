module Std.IO.StdIO

{-|
Module      : Std.IO.TTY
Description : TTY devices
Copyright   : (c) Winterland, 2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provides stdin\/stderr\/stdout reading and writings.
-}


-- TODO: use uv_guess_handle to correct handle regular file std streams
--

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
