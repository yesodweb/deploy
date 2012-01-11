{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment (getEnv)

main :: IO ()
main = do
    portS <- getEnv "PORT"
    port <- return $ read portS
    run port app

app :: Application
app _ = return $ responseLBS status200 [("Content-Type", "text/html")]
    "<html><head><link rel='stylesheet' href='http://static.testapp1/style.css'></head><body><h1>Welcome to testapp1</h1></body></html>"
