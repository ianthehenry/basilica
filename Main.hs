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

app :: Database -> Sockets.Broadcaster -> IO Application
app db broadcast = scottyApp $ do
  get "/threads/:id" $
    param "id" >>= withThread json
  get "/threads" $
    liftIO (listThreads db) >>= json
  post "/threads" $
    flip rescue (\msg -> status status400 >> text msg) $ do
      [title, creator] <- sequence $ param <$> ["title", "username"]
      newThread <- liftIO (createThread db title creator Nothing)
      liftIO $ broadcast newThread
      json newThread

  where
    withThread f idThread =
      maybe thread404 f =<< liftIO (getThread db idThread)
    withThread :: (Thread -> ActionM ()) -> ID -> ActionM ()
    thread404 = status status404 >> text "Thread not found"

main :: IO ()
main = do
  let port = 3000
  database <- newDatabase
  (broadcast, server) <- Sockets.newServer
  api <- app database broadcast
  putStrLn $ "Running on port " ++ show port
  Warp.run port (websocketsOr defaultConnectionOptions server api)
