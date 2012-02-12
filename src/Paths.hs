{-# LANGUAGE OverloadedStrings #-}
-- | Find paths given the root directory of deployment.
module Paths
    ( RootDir
    , incoming
    , angelConf
    , nginxConf
    , postgresConf
    ) where

import Prelude ()
import Filesystem.Path.CurrentOS (FilePath, (</>))

type RootDir = FilePath

incoming :: RootDir -> FilePath
incoming = (</> "incoming")

angelConf :: RootDir -> FilePath
angelConf r = r </> "etc" </> "angel.conf"

nginxConf :: FilePath
nginxConf = "/etc/nginx/sites-enabled/yesod-deploy.conf"

postgresConf :: RootDir -> FilePath
postgresConf r = r </> "etc" </> "postgres.yaml"
