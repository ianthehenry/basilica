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
  post "/threads" $ accessControl $ 
    with400 $ threadPost (return Nothing)
  post "/threads/:id" $ accessControl $
    with400 $ threadPost (Just <$> param "id")
  where
    withThread f idThread = do
      liftIO $ print idThread
      maybe thread404 f =<< liftIO (getThread db idThread)
    thread404 = status status404 >> text "Thread not found"
    with400 a = rescue a (\msg -> status status400 >> text msg)
    threadPost idParent = do
      maybeThread <- liftIO =<< makeThread <$> idParent <*> param "by" <*> param "content"
      case maybeThread of
        Just thread -> json thread
        Nothing     -> thread404
    makeThread idParent by content = do
      newThread <- createThread db by content idParent
      whenMaybe newThread broadcast
      return newThread
    whenMaybe = flip (maybe (return ()))

main :: IO ()
main = do
  let port = 3000
  database <- newDatabase
  (broadcast, server) <- Sockets.newServer
  api <- app database broadcast
  putStrLn $ "Running on port " ++ show port
  Warp.run port (websocketsOr defaultConnectionOptions server api)
