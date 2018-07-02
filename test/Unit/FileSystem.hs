{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.FileSystem where

import Data.Bits
import Data.Vector as V
import Test.Tasty hiding (withResource)
import Test.Tasty.HUnit
import Control.Concurrent.MVar (readMVar)
import Foreign.Marshal.Array
import Foreign.Ptr
import System.IO.Exception
import System.IO.FileSystem as FS
import System.IO.Resource
import System.IO.Buffered
import System.IO.UV.Manager

unitFileSystem :: TestTree
unitFileSystem = testGroup "filesystem operations" [
    testCase "Opens and writes a file" $ do

        let flags = uV_FS_O_RDWR .|. uV_FS_O_CREAT
            mode = defaultMode
            filename = "stdio-unit"

            content = [vASCII|Hello world!|]
            content2 = [vASCII|quick fox jumps over the lazy dog|]
            size = V.length content
            size2 = V.length content2

        withResource (initUVFile filename flags mode) $ \ file -> do
            o <- newBufferedOutput file 4096
            writeBuffer o content
            flush o

        withResource (initUVFile filename flags mode) $ \ file -> do
            i <- newBufferedInput file 4096
            written <- readExactly size i
            written @=? content

        withResource (initUVFileT filename flags mode) $ \ file -> do
            o <- newBufferedOutput file 4096
            writeBuffer o content2
            flush o

        withResource (initUVFileT filename flags mode) $ \ file -> do
            i <- newBufferedInput file 4096
            written <- readExactly size2 i
            written @=? content2

        unlink filename

    ]
