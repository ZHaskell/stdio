{-# LANGUAGE OverloadedStrings #-}

module Std.IO.FileSystemSpec where

import           Control.Concurrent.MVar (readMVar)
import           Control.Monad
import           Data.Bits
import           Std.Data.Vector         as V
import           Std.Data.Vector.Base    as V
import           Data.List               as List
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Std.IO.Buffered
import           Std.IO.Exception
import           Std.IO.FileSystem
import           Std.IO.Resource
import           Std.IO.UV.Manager
import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = describe "filesystem operations" $ do

        let content = "Hello world!"
            content2 = V.cycleN 1024 "quick fox jumps over the lazy dog, 世界你好!\n"
            size = V.length content
            size2 = V.length content2

        tempdir <- runIO $ mkdtemp "stdio-filesystem-unit"

        it "create a temp dir" $ do

            dirs <- scandir "./"
            List.lookup tempdir dirs @?= Just DirEntDir


        let flags = O_RDWR .|. O_CREAT
            mode = DEFAULT_MODE
            filename = tempdir <> "/test-file"

        it "Opens and writes a file" $ do
            withResource (initUVFile filename flags mode) $ \ file -> do
                o <- newBufferedOutput file 4096
                writeBuffer o content
                flushBuffer o

            withResource (initUVFile filename flags mode) $ \ file -> do
                i <- newBufferedInput file 4096
                written <- readExactly size i
                written @=? content

                fr <- newUVFileReader file 0
                i <- newBufferedInput fr 4096
                written <- readExactly size i
                written @=? content

            unlink filename

        it "Opens and writes a file II" $ do
            withResource (initUVFile filename flags mode) $ \ file -> do
                o <- newBufferedOutput file 4096
                writeBuffer o content2
                flushBuffer o

            withResource (initUVFile filename flags mode) $ \ file -> do
                i <- newBufferedInput file 4096
                written <- readExactly size2 i
                written @=? content2

            withResource (initUVFile filename flags mode) $ \ file -> do
                i <- newBufferedInput file 4096
                firstLine <- readLine i
                firstLine  @=? fst (V.break (== V.c2w '\n') content2) `V.snoc` (V.c2w '\n')

                fr <- newUVFileReader file 0
                i <- newBufferedInput fr 4096
                replicateM_ 1024 $ do
                    firstLine <- readLine i
                    firstLine  @=? fst (V.break (== V.c2w '\n') content2) `V.snoc` (V.c2w '\n')
            unlink filename

        let dirname  = tempdir <> "/test-dir"

        it "create and remove dir" $ do
            mkdir dirname mode
            dirs <- scandir tempdir
            print dirs
            List.lookup "test-dir" dirs @?= Just DirEntDir
            rmdir dirname

        let linkname  = tempdir <> "/test-link"
            symlinkname  = tempdir <> "/test-symlink"
            symlinkname2  = tempdir <> "/test-symlink2"

        it "link stat should be equal to target file" $ do

            withResource (initUVFile filename flags mode) $ \ file -> return ()

            s0 <- stat filename

            link filename linkname
            symlink "test-link" symlinkname SYMLINK_DEFAULT

            absfp <- realpath filename
            symlink absfp symlinkname2 SYMLINK_DEFAULT  -- the second way to create a proper symlink

            s1 <- stat linkname
            s2 <- stat symlinkname
            s2' <- stat symlinkname2

            s0 @?= s1 {stNlink = 1} -- update hard link number
            s0 @?= s2 {stNlink = 1}
            s0 @?= s2' {stNlink = 1}

            withResource (initUVFile filename flags mode) $ \ file -> do
                s4 <- fstat file
                s0 @?= s4 {stNlink = 1}

            unlink filename
            unlink linkname
            unlink symlinkname
            unlink symlinkname2

        it "utime result in stat change" $ do
            withResource (initUVFile filename flags mode) $ \ file -> return ()
            utime filename 1000.2000 3000.4000
            s <- stat filename
            print s
            uvtSecond (stAtim s) @?= 1000
            uvtNanoSecond (stAtim s) @?= 200000000
            uvtSecond (stMtim s) @?= 3000
            uvtNanoSecond (stMtim s) @?= 400000000
            unlink filename

        it "futime result in fstat change" $ do
            withResource (initUVFile filename flags mode) $ \ file -> do
                futime file 5000.6000 7000.8000
                s <- fstat file
                print s
                uvtSecond (stAtim s) @?= 5000
                uvtNanoSecond (stAtim s) @?= 600000000
                uvtSecond (stMtim s) @?= 7000
                uvtNanoSecond (stMtim s) @?= 800000000
            unlink filename

        it "remove test temp dir" $ rmdir tempdir
