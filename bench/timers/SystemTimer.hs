module Main where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import GHC.Event

main :: IO ()
main = do
    r <- newTVarIO 0 :: IO (TVar Int)

    tm <- getSystemTimerManager

    replicateM 100000 . forkIO $ do
         forM_ [1..10] $ \ i -> do
            registerTimeout tm (i*1000000) (atomically $ modifyTVar' r (+1))

    atomically $ do
        r' <- readTVar r
        unless (r' == 1000000) retry

