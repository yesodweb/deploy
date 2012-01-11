{-# LANGUAGE OverloadedStrings #-}
-- | Reload config data and write new config files.
module Reload
    ( reload
    ) where

import Prelude hiding (FilePath, writeFile)
import System.Cmd (rawSystem)
import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Filesystem.Path.CurrentOS
    ( FilePath, (</>), encodeString
    )
import Filesystem (rename)

import Config
import Angel
import Nginx
import Paths

reload :: RootDir -> FilePath -> IO ()
reload rootDir unpackedFolder = do
    deploys <- loadDeploys unpackedFolder
    writeFile rootDir (angelConf rootDir) $ angelFile rootDir deploys
    writeFile rootDir nginxConf $ nginxFile deploys
    _ <- rawSystem "reload" ["yesod-deploy-angel"]
    _ <- rawSystem "/etc/init.d/nginx" ["reload"]
    return ()

writeFile :: RootDir -> FilePath -> Builder -> IO ()
writeFile rootDir file contents = do
    let fp = rootDir </> "tmp"
    L.writeFile (encodeString fp) $ encodeUtf8 $ toLazyText contents
    rename fp file
