{-# LANGUAGE OverloadedStrings #-}

module Std.IO.FileSystemSpec where

import           Control.Concurrent.MVar (readMVar)
import           Data.Bits
import           Std.Data.Vector         as V
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Std.IO.Buffered
import           Std.IO.Exception
import           Std.IO.FileSystem       as FS
import qualified Std.IO.FileSystemT      as FST
import           Std.IO.Resource
import           Std.IO.UV.Manager
import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = describe "filesystem operations" $ do
    it "Opens and writes a file" $ do

        let flags = uV_FS_O_RDWR .|. uV_FS_O_CREAT
            mode = defaultMode
            filename = "stdio-unit"

            content = "Hello world!"
            content2 = "quick fox jumps over the lazy dog"
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

        withResource (FST.initUVFile filename flags mode) $ \ file -> do
            o <- newBufferedOutput file 4096
            writeBuffer o content2
            flush o

        withResource (FST.initUVFile filename flags mode) $ \ file -> do
            i <- newBufferedInput file 4096
            written <- readExactly size2 i
            written @=? content2

        unlink filename
