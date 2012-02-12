{-# LANGUAGE OverloadedStrings #-}
-- | Generate an Angel config file.
module Angel
    ( angelFile
    ) where

import Prelude hiding (FilePath)
import Data.Monoid (Monoid, mappend, mconcat)
import Data.Text.Lazy.Builder (Builder, fromText, fromString, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Filesystem.Path.CurrentOS (encodeString)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans.State (StateT, runStateT, get, put)
import Control.Monad.IO.Class (liftIO)
import Data.Yaml (decodeFile, encodeFile)
import Control.Monad (unless, replicateM)
import System.Random (randomRIO)
import Config
import Paths
import Filesystem (isFile)
import System.Process (readProcess)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- | A block in an angel file for an individual webapp.
angelBlock :: WebappFinal -> Builder
angelBlock (WebappFinal w d p mpost) =
    fromText (deployName d) <> "-" <> fromText (webappHost w) <> " {\n" <>
    "    exec = \"env PORT=" <> decimal p <> " " <> postgresEnv <> fromString (encodeString $ webappExec w) <> "\"\n" <>
    "    directory = \"" <> fromString (encodeString $ deployDirectory d) <> "\"\n" <>
    "}\n\n"
  where
    postgresEnv =
        case mpost of
            Nothing -> ""
            Just (Postgres user pass db) ->
                "PGHOST=localhost PGPORT=5432 PGUSER=" <>
                fromText user <>
                " PGPASS=" <>
                fromText pass <>
                " PGDATABASE=" <>
                fromText db <>
                " "

-- | Generate the deploy block, for monitoring this program.
deployBlock :: RootDir -> Builder
deployBlock rootDir =
    "deploy {\n" <>
    "    exec = \"" <> rootDir' <> "bin/deploy " <> rootDir' <> "\"\n" <>
    "}\n\n"
  where
    rootDir' = fromString $ encodeString rootDir

-- | The full text of the angel config file.
angelFile :: RootDir -> Deploys -> IO Builder
angelFile rootDir ds = do
    wafs <- addPostgres $ webappFinals ds
    return $ deployBlock rootDir <> mconcat (map angelBlock wafs)
  where
    addPostgres :: [WebappFinal] -> IO [WebappFinal]
    addPostgres wafs' = do
        let fp = postgresConf rootDir
            fp' = encodeString fp
        exists <- isFile fp
        pgs0 <-
            if exists
                then decodeFile fp' >>= maybe (error $ "Invalid PostgreSQL YAML file") return
                else return Map.empty
        (wafs, pgs) <- runStateT (mapM addPostgres' wafs') pgs0
        unless (pgs == pgs0) $ encodeFile (encodeString $ postgresConf rootDir) pgs
        return wafs

    addPostgres' :: WebappFinal -> StateT (Map.Map Text Postgres) IO WebappFinal
    addPostgres' waf'
        | deployPostgresql $ wafDeploy waf' = do
            let name = deployName $ wafDeploy waf'
            pgs <- get
            pg <-
                case Map.lookup name pgs of
                    Just pg -> return pg
                    Nothing -> do
                        pass <- liftIO $ replicateM 10 $ randomRIO ('A', 'Z')
                        suffix <- liftIO $ replicateM 5 $ randomRIO ('a', 'z')
                        let user = pack $ "yesod_deploy_" ++ unpack name ++ suffix
                            db = user
                        let pg = Postgres user (pack pass) db
                        let sql = TL.unpack $ toLazyText $ toSql pg
                        res <- liftIO $ readProcess
                            "/usr/bin/sudo"
                            ["-u", "postgres", "psql"]
                            sql
                        liftIO $ putStrLn res
                        put $ Map.insert name pg pgs
                        return pg
            return $ waf' { wafPostgres = Just pg }
        | otherwise = return waf'
    toSql (Postgres user pass db) =
        "CREATE USER " <> fromText user <> " PASSWORD '" <> fromText pass <> "';\n" <>
        "CREATE DATABASE " <> fromText db <> " OWNER " <> fromText user <> ";"
