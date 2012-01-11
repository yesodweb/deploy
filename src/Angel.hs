{-# LANGUAGE OverloadedStrings #-}
-- | Generate an Angel config file.
module Angel
    ( angelFile
    ) where

import Prelude hiding (FilePath)
import Data.Monoid (Monoid, mappend, mconcat)
import Data.Text.Lazy.Builder (Builder, fromText, fromString)
import Data.Text.Lazy.Builder.Int (decimal)
import Filesystem.Path.CurrentOS (encodeString)
import Config
import Paths

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- | A block in an angel file for an individual webapp.
angelBlock :: WebappPort -> Builder
angelBlock (WebappPort w d p) =
    fromText (deployName d) <> "-" <> fromText (webappHost w) <> " {\n" <>
    "    exec = \"env PORT=" <> decimal p <> " " <> fromString (encodeString $ webappExec w) <> "\"\n" <>
    "    directory = \"" <> fromString (encodeString $ deployDirectory d) <> "\"\n" <>
    "}\n\n"

-- | Generate the deploy block, for monitoring this program.
deployBlock :: RootDir -> Builder
deployBlock rootDir =
    "deploy {\n" <>
    "    exec = \"" <> rootDir' <> "bin/deploy " <> rootDir' <> "\"\n" <>
    "}\n\n"
  where
    rootDir' = fromString $ encodeString rootDir

-- | The full text of the angel config file.
angelFile :: RootDir -> Deploys -> Builder
angelFile rootDir ds = deployBlock rootDir <> mconcat (map angelBlock $ webappPorts ds)
