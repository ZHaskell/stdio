module Unit.FileSystem where

import Data.Char (chr, ord)
import Test.Tasty hiding (withResource)
import Test.Tasty.HUnit
import Control.Concurrent.MVar (readMVar)
import Foreign.Marshal.Array
import Foreign.Ptr
import System.IO.Exception
import System.IO.FileSystem as FS
import System.IO.Resource
import System.IO.Temp
import System.IO.UV.Manager
import System.IO.UV.Internal

unitFileSystem :: TestTree
unitFileSystem = testGroup "filesystem operations" [
        testCase "Opens and writes a file" $ do
            
            let flags = uV_FS_O_RDWR
                mode = defaultMode
                content = "Hello world!"
                size = length content
            filename <- emptySystemTempFile "stdio-unit"

            withResource (open filename flags mode) $ \fd ->
                withArray (map (fromIntegral . ord) content) $ \buf ->
                    write fd buf size 0

            written <- readFile filename
            written @=? content

    ,   testCase "Reads a file" $ do

            let flags = uV_FS_O_RDWR
                mode = defaultMode
                content = "Hello world!"
                size = length content
            filename <- writeSystemTempFile "stdio-unit" content

            withResource (open filename flags mode) $ \fd ->
                withResource (FS.read fd size 0) $ \buf -> do
                    res <- map (chr . fromIntegral) <$> peekArray size buf
                    res @=? content

    ,   testCase "Reads a using the threaded version" $ do

            let flags = uV_FS_O_RDONLY
                mode = defaultMode
                content = "Hello world!"
                size = length content
            filename <- writeSystemTempFile "stdio-unit" content

            uvm <- getUVManager

            withResource (open filename flags mode) $ \fd ->
                withResource (readT uvm fd size 0) $ \(req, buf) -> do
                    slot <- peekUVReqData $ castPtr req
                    mvar <- withUVManager' uvm $ getBlockMVar uvm slot

                    readMVar mvar
                    readLength <- throwUVIfMinus $
                        fromIntegral <$> peekUVFSReqResult req

                    res <- map (chr . fromIntegral) <$> peekArray readLength buf
                    res @=? content
    ]
