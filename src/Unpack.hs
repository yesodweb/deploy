{-# LANGUAGE OverloadedStrings #-}
-- | Unpack the files from incoming.
module Unpack
    ( unpack
    ) where

import Prelude hiding (FilePath, catch)
import Control.Exception (catch, throwIO)
import System.IO.Error (isAlreadyExistsError)
import System.Cmd (rawSystem)
import Filesystem.Path.CurrentOS
    ( FilePath, (</>), decodeString, hasExtension, basename, encodeString
    )
import Filesystem (listDirectory, createDirectory, isFile)
import Paths (RootDir, incoming)
import Reload (reload)

unpack :: RootDir -> IO ()
unpack rootDir = do
    dest <- getDest [1..]
    listDirectory (incoming rootDir) >>= mapM_ (unpack1 dest)
    reload rootDir dest
  where
    -- find the next unused folder
    getDest :: [Int] -> IO FilePath
    getDest [] = error "getDest:impossible happened"
    getDest (i:is) = do
        let dest = rootDir </> "unpacked" </> decodeString (show i)
        (createDirectory False dest >> return dest) `catch`
            \e -> if isAlreadyExistsError e then getDest is else throwIO e

    -- unpacks a single file, assuming it has a .yesod extension
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
