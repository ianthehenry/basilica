module Main where

import           BasePrelude hiding (app)
import           Control.Monad.Trans (liftIO)
import           Database
import           Network.HTTP.Types
import           Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.WebSockets.Connection (defaultConnectionOptions)
import qualified Sockets
import           Web.Scotty

accessControl :: ActionM () -> ActionM ()
accessControl = (>> setHeader "Access-Control-Allow-Origin" "*")

app :: Database -> Sockets.Broadcaster -> IO Application
app db broadcast = scottyApp $ do
  get "/threads/:id" $ accessControl $
    withThread json =<< param "id"
  get "/threads" $ accessControl $
    liftIO (allThreads db) >>= json
  post "/threads/:id" $ accessControl $
    flip rescue (\msg -> status status400 >> text msg) $ do
      idParent <- param "id"
      [content, by] <- sequence $ param <$> ["content", "by"]
      flip withThread idParent $ \parent -> do
        newThread <- liftIO (createThread db by content parent)
        liftIO $ broadcast newThread
        json newThread
  where
    withThread f idThread = do
      liftIO $ print idThread
      maybe thread404 f =<< liftIO (getThread db idThread)
    thread404 = status status404 >> text "Thread not found"

main :: IO ()
main = do
  let port = 3000
  database <- newDatabase
  (broadcast, server) <- Sockets.newServer
  api <- app database broadcast
  putStrLn $ "Running on port " ++ show port
  Warp.run port (websocketsOr defaultConnectionOptions server api)
