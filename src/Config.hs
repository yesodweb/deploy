{-# LANGUAGE OverloadedStrings #-}
-- | Load the config data from the individual deploy.yaml files.
module Config
    ( Deploys
    , Deploy (..)
    , Webapp (..)
    , Static (..)
    , Postgres (..)
    , loadDeploys
    , WebappFinal (..)
    , webappFinals
    ) where

import Prelude hiding (FilePath, writeFile)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM, mzero)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

import Data.Text (Text)
import Data.Yaml
import Filesystem.Path.CurrentOS
    ( FilePath, fromText, (</>), directory, encodeString
    )
import Filesystem (canonicalizePath, listDirectory, isDirectory, isFile)

-- | A single deployment bundle.
data Deploy = Deploy
    { deployName :: Text
    , deployDirectory :: FilePath
    , deployWebapps :: [Webapp]
    , deployStatics :: [Static]
    , deployPostgresql :: Bool
    }
    deriving (Eq, Show)

-- | One webapp within a bundle.
data Webapp = Webapp
    { webappHost :: Text
    , webappExec :: FilePath
    }
    deriving (Eq, Show)

-- | A static folder to be served.
data Static = Static
    { staticHost :: Text
    , staticDirectory :: FilePath
    }
    deriving (Eq, Show)

instance FromJSON Deploy where
    parseJSON (Object o) = Deploy
        <$> o .: "name"
        <*> return ""
        <*> o .:? "webapps"    .!= []
        <*> o .:? "statics"    .!= []
        <*> o .:? "postgresql" .!= False
    parseJSON _ = mzero

instance FromJSON Webapp where
    parseJSON (Object o) = Webapp
        <$> o .: "host"
        <*> (fromText <$> o .: "exec")
    parseJSON _ = mzero

instance FromJSON Static where
    parseJSON (Object o) = Static
        <$> o .: "host"
        <*> (fromText <$> o .: "directory")
    parseJSON _ = mzero

-- | Load a 'Deploy' from the given file. All paths returned are absolute and
-- canonicalized.
loadDeploy :: FilePath -> IO Deploy
loadDeploy fp = do
    putStrLn $ "Loading deploy config from: " ++ show fp
    mdeploy <- decodeFile $ encodeString fp
    deploy <- maybe (error $ "Invalid deploy file: " ++ show fp) return mdeploy
    dir <- canonicalizePath $ directory fp
    makeAbsolute deploy { deployDirectory = dir }

-- | Turn the relative paths in a 'Deploy' into absolute, canonical paths.
makeAbsolute :: Deploy -> IO Deploy
makeAbsolute (Deploy name dir ws ss post) =
    Deploy <$> return name
           <*> return dir
           <*> mapM goW ws
           <*> mapM goS ss
           <*> return post
  where
    goW (Webapp h e) = do
        path <- canonicalizePath $ dir </> e
        return $ Webapp h path
    goS (Static h d) = do
        path <- canonicalizePath $ dir </> d
        return $ Static h path

type Deploys = Map.Map Text Deploy

-- | Load all the deployment information from the given folder.
loadDeploys :: FilePath -> IO Deploys
loadDeploys root = do
    contents <- listDirectory root
    deploys <- catMaybes <$> mapM go contents
    foldM addDeploy Map.empty deploys
  where
    go folder = do
        isD <- isDirectory folder
        if isD
            then do
                let fp = folder </> "deploy.yaml"
                isF <- isFile fp
                if isF
                    then Just <$> loadDeploy fp
                    else return Nothing
            else return Nothing
    addDeploy m d =
        case Map.lookup (deployName d) m of
            Nothing -> return $ Map.insert (deployName d) d m
            Just _ -> error $ "Duplicate name: " ++ show (deployName d)

-- | Full information on a single webapp
data WebappFinal = WebappFinal
    { wafWebapp   :: Webapp
    , wafDeploy   :: Deploy
    , wafPort     :: Int
    , wafPostgres :: Maybe Postgres
    }
    deriving (Show, Eq)

-- | PostgreSQL settings.
data Postgres = Postgres
    { pgUser :: Text
    , pgPass :: Text
    , pgDB   :: Text
    }
    deriving (Show, Eq)

instance ToJSON Postgres where
    toJSON (Postgres u p d) = object
        [ "user" .= u
        , "pass" .= p
        , "db"   .= d
        ]

instance FromJSON Postgres where
    parseJSON (Object v) = Postgres
        <$> v .: "user"
        <*> v .: "pass"
        <*> v .: "db"
    parseJSON _ = mzero

-- | Get all the 'WebappFinal's from the full config information.
--
-- Note: This will not include any PostgreSQL settings
webappFinals :: Deploys -> [WebappFinal]
webappFinals m =
    map go $ zip webapps [4000..]
  where
    webapps = concatMap (\d -> zip (repeat d) (deployWebapps d))
            $ Map.elems m
    go ((d, w), p) = WebappFinal w d p Nothing
