{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import System.INotify
import Control.Concurrent
import Control.Monad
import Control.Exception
import Prelude hiding (catch, FilePath)
import Filesystem.Path.CurrentOS
import Filesystem
import System.IO.Error (isAlreadyExistsError)
import System.Cmd (rawSystem)

main :: IO ()
main = do
    args <- getArgs
    dir <-
        case args of
            [a] -> return a
            _ -> error "Invalid args"
    let dir' = decodeString dir
    unpack dir'
    inotify <- initINotify
    poll <- newEmptyMVar
    _ <- addWatch inotify [AllEvents] (dir ++ "incoming") (const $ putMVar poll ())
    takeMVar poll

unpack :: FilePath -> IO ()
unpack dir = do
    dest <- getDest [1..]
    let incoming = dir </> "incoming"
    listDirectory incoming >>= mapM_ (unpack1 dest)
    _ <- rawSystem (encodeString $ dir </> "bin" </> "reconfig")
        [ encodeString dir
        , encodeString dest
        , encodeString $ dir </> "etc" </> "angel.conf"
        , "/etc/nginx/sites-enabled/yesod-deploy.conf"
        ]
    return ()
  where
    getDest :: [Int] -> IO FilePath
    getDest [] = error "getDest:impossible happened"
    getDest (i:is) = do
        let dest = dir </> "unpacked" </> decodeString (show i)
        (createDirectory False dest >> return dest) `catch`
            \e -> if isAlreadyExistsError e then getDest is else throwIO e
    unpack1 dest file = do
        isF <- isFile file
        if isF && hasExtension file "yesod"
            then do
                let dest' = dest </> basename file
                createDirectory True dest'
                _ <- rawSystem "tar"
                    [ "zxfC"
                    , encodeString file
                    , encodeString dest'
                    ]
                return ()
            else return ()
