{-# LANGUAGE OverloadedStrings #-}
import Data.Yaml
import Data.Text (Text, unpack)
import Filesystem.Path.CurrentOS
    ( FilePath, fromText, (</>), directory, encodeString, decodeString
    )
import Filesystem
    ( listDirectory, isFile, isDirectory, canonicalizePath, rename
    )
import Prelude hiding (FilePath, writeFile)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, foldM)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import System.Cmd (rawSystem)
import System.Environment (getArgs)
import qualified Prelude

data Deploy = Deploy
    { deployName :: Text
    , deployDirectory :: FilePath
    , deployWebapps :: [Webapp]
    , deployStatics :: [Static]
    }
    deriving Show

instance FromJSON Deploy where
    parseJSON (Object o) = Deploy
        <$> o .: "name"
        <*> return ""
        <*> o .: "webapps"
        <*> o .: "statics"
    parseJSON _ = mzero

data Webapp = Webapp
    { webappHost :: Text
    , webappExec :: FilePath
    }
    deriving Show

instance FromJSON Webapp where
    parseJSON (Object o) = Webapp
        <$> o .: "host"
        <*> (fromText <$> o .: "exec")
    parseJSON _ = mzero

data Static = Static
    { staticHost :: Text
    , staticDirectory :: FilePath
    }
    deriving Show

instance FromJSON Static where
    parseJSON (Object o) = Static
        <$> o .: "host"
        <*> (fromText <$> o .: "directory")
    parseJSON _ = mzero

loadDeploy :: FilePath -> IO Deploy
loadDeploy fp = do
    putStrLn $ "Loading deploy config from: " ++ show fp
    Just deploy <- decodeFile $ encodeString fp
    dir <- canonicalizePath $ directory fp
    makeAbsolute deploy { deployDirectory = dir }

makeAbsolute :: Deploy -> IO Deploy
makeAbsolute (Deploy name dir ws ss) =
    Deploy <$> return name
           <*> return dir
           <*> mapM goW ws
           <*> mapM goS ss
  where
    goW (Webapp h e) = do
        path <- canonicalizePath $ dir </> e
        return $ Webapp h path
    goS (Static h d) = do
        path <- canonicalizePath $ dir </> d
        return $ Static h path

loadDeploys :: FilePath -> IO (Map.Map Text Deploy)
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

webappsPorts :: Map.Map Text Deploy -> [((Deploy, Webapp), Int)]
webappsPorts m = zip webapps [4000..]
  where
    webapps = concatMap (\d -> zip (repeat d) (deployWebapps d))
            $ Map.elems m

angelBlock :: ((Deploy, Webapp), Int) -> String
angelBlock ((d, w), p) = unlines
    [ concat [unpack $ deployName d, "-", unpack $ webappHost w, " {"]
    , concat
        [ "    exec = \"env PORT="
        , show p
        , " "
        , encodeString $ webappExec w
        , "\""
        ]
    , concat
        [ "    directory = \""
        , encodeString $ deployDirectory d
        , "\""
        ]
    , "}"
    ]

nginxBlockWebapp :: ((Deploy, Webapp), Int) -> String
nginxBlockWebapp ((_, w), p) = unlines
    [ "server {"
    , concat ["    server_name ", unpack $ webappHost w, ";"]
    , "    location / {"
    , concat
        [ "        proxy_pass http://127.0.0.1:"
        , show p
        , ";"
        ]
    , "    }"
    , "}"
    ]

nginxBlockStatic :: Static -> String
nginxBlockStatic s = unlines
    [ "server {"
    , concat ["    server_name ", unpack $ staticHost s, ";"]
    , concat
        [ "        root "
        , encodeString $ staticDirectory s
        , ";"
        ]
    , "}"
    ]

main :: IO ()
main = do
    args <- getArgs
    (rootDir, unpackedFolder, angelConfig, nginxConfig) <-
        case args of
            [a, b, c, d] -> return (a, b, c, d)
            _ -> error "Invalid arguments"
    let unpacker = unlines
            [ "unpacker {"
            , concat
                [ "    exec = \""
                , rootDir
                , "bin/unpacker "
                , rootDir
                , "\""
                ]
            , "}"
            ]
    deploys <- loadDeploys $ decodeString unpackedFolder
    let was = webappsPorts deploys
    let statics = concatMap deployStatics $ Map.elems deploys
    let rootDir' = decodeString rootDir
    writeFile rootDir' angelConfig
        $ unpacker ++ concatMap angelBlock was
    writeFile rootDir' nginxConfig
        $ concatMap nginxBlockWebapp was ++
          concatMap nginxBlockStatic statics
    _ <- rawSystem "reload" ["yesod-deploy-angel"]
    _ <- rawSystem "/etc/init.d/nginx" ["reload"]
    return ()

writeFile :: FilePath -> String -> String -> IO ()
writeFile rootDir file contents = do
    let fp = rootDir </> "tmp"
    Prelude.writeFile (encodeString fp) contents
    rename fp $ decodeString file
