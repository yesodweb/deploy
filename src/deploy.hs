{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import System.INotify (initINotify, addWatch, EventVariety (AllEvents))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Prelude hiding (catch, FilePath)
import Filesystem.Path.CurrentOS (encodeString, decodeString)
import Unpack (unpack)
import Paths (incoming)

main :: IO ()
main = do
    args <- getArgs
    rootDir <-
        case args of
            [a] -> return $ decodeString a
            _ -> error "Invalid args"

    -- Unpack, then wait until there's any activity in the incoming folder.
    -- When this process dies, angel will restart it, allowing it to unpack
    -- again. We do the unpack first so that when the code is first loaded, we
    -- get an initial unpack even without filesystem activity.
    unpack rootDir
    inotify <- initINotify
    poll <- newEmptyMVar
    _ <- addWatch inotify [AllEvents] (encodeString $ incoming rootDir) (const $ putMVar poll ())
    takeMVar poll
