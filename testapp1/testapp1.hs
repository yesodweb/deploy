{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment (getEnv)
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    portS <- getEnv "PORT"
    phost <- getEnv "PGHOST"
    pport <- getEnv "PGPORT"
    puser <- getEnv "PGUSER"
    ppass <- getEnv "PGPASS"
    pdb <- getEnv "PGDATABASE"
    port <- return $ read portS
    run port $ app $ show (phost, pport, puser, ppass, pdb)

app :: String -> Application
app conn _ = return $ responseLBS status200 [("Content-Type", "text/html")] $ L8.pack $ concat
    [ "<html><head><link rel='stylesheet' href='http://static.testapp1/style.css'></head><body><h1>Welcome to testapp1</h1><p>DB info:"
    , conn
    , "</p></body></html>"
    ]
