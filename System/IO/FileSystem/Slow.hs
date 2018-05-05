module System.IO.FileSystem.Slow where

import System.IO.Exception
import System.IO.UV.Internal
import System.IO.UV.Manager
import System.IO.UV.Exception
import Control.Concurrent.MVar
import Foreign.Ptr
import Foreign.C
import Data.CBytes

scandir :: CBytes -> IO [(CBytes, UVDirEntType)]
scandir path = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withResource (initUVSlot uvm) $ \ slot ->
            withResource (initUVReq uV_FS) $ \ req -> do
                lock <- getBlockMVar uvm slot
                pokeUVReqData req slot
                withUVManager uvm $ \ loop -> uvFSScandir loop req p True
                takeMVar lock
                go req
  where
    go req = do
        withResource initUVDirEnt $ \ ent -> do
            r <- uv_fs_scandir_next req ent
            if r == uV_EOF
            then return []
            else if r < 0
                then do
                    throwUVIfMinus_ $ return r
                    return []
                else do
                    (path, typ) <- peekUVDirEnt ent
                    path' <- fromCString path
                    rest <- go req
                    return ((path', typ) : rest)



