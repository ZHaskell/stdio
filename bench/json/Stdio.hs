{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Std.Data.JSON.Value as JSON
import Std.IO.Buffered
import Std.IO.FileSystem
import Std.IO.Resource
import Std.Data.CBytes as CBytes
import Control.Monad
import Data.Time.Clock
import Std.Data.Vector.Base as V
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  (bs:cnt:args) <- getArgs
  let count = read cnt :: Int
      blkSize = read bs :: Int
  forM_ args $ \arg -> withResource (initUVFile (CBytes.pack arg) O_RDWR DEFAULT_MODE) $ \ f -> do
    putStrLn $ arg ++ ":"
    start <- getCurrentTime
    r <- newUVFileReader f 0
    bio <- newBufferedInput r blkSize
    let loop !good !bad
            | good+bad >= count = return (good, bad)
            | otherwise = do
          peekUVFileReader r 0
          (_, result) <- parseValueChunks (readBuffer bio) =<< (readBuffer bio)
          case result of
            Right _ -> loop (good+1) bad
            _       -> loop good (bad+1)
    (good, _) <- loop 0 0
    delta <- flip diffUTCTime start `fmap` getCurrentTime
    putStrLn $ "  " ++ show good ++ " good, " ++ show delta
    let rate = fromIntegral count / realToFrac delta :: Double
    putStrLn $ "  " ++ show (round rate :: Int) ++ " per second"
