{-# LANGUAGE OverloadedStrings #-}
-- | Generate an Nginx config file.
module Nginx
    ( nginxFile
    ) where

import Prelude hiding (FilePath)
import Data.Monoid (Monoid, mappend, mconcat)
import qualified Data.Map as Map
import Data.Text.Lazy.Builder (Builder, fromText, fromString)
import Data.Text.Lazy.Builder.Int (decimal)
import Filesystem.Path.CurrentOS (encodeString)
import Config

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

nginxBlockWebapp :: WebappPort -> Builder
nginxBlockWebapp (WebappPort w _ p) =
    "server {\n" <>
    "    server_name " <> fromText (webappHost w) <> ";\n" <>
    "    location / {\n" <>
    "        proxy_pass http://127.0.0.1:" <> decimal p <> ";\n" <>
    "    }\n" <>
    "}\n\n"

nginxBlockStatic :: Static -> Builder
nginxBlockStatic s =
    "server {\n" <>
    "    server_name " <> fromText (staticHost s) <> ";\n" <>
    "    root " <> fromString (encodeString $ staticDirectory s) <> ";\n" <>
    "}\n\n"

nginxFile :: Deploys -> Builder
nginxFile ds = mconcat (map nginxBlockWebapp $ webappPorts ds) <>
               mconcat (map nginxBlockStatic $ concatMap deployStatics $ Map.elems ds)
