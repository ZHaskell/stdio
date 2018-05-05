module Main where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import System.LowResTimer

main :: IO ()
main = do
    r <- newTVarIO 0 :: IO (TVar Int)

    replicateM 100000 . forkIO $ do
         forM_ [1..10] $ \ i -> do
            registerLowResTimer (i*10) (atomically $ modifyTVar' r (+1))

    atomically $ do
        r' <- readTVar r
        unless (r' == 1000000) retry

