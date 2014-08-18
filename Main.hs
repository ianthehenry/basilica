module Main where

import           BasePrelude hiding (app)
import           Control.Monad.Trans (liftIO)
import qualified Data.Configurator as Conf
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
  get "/posts/:id" $ accessControl $
    withPost json =<< param "id"
  get "/posts" $ accessControl $
    liftIO (allPosts db) >>= json
  post "/posts" $ accessControl $
    with400 $ postPost (return Nothing)
  post "/posts/:id" $ accessControl $
    with400 $ postPost (Just <$> param "id")
  where
    withPost f idPost = do
      liftIO $ print idPost
      maybe post404 f =<< liftIO (getPost db idPost)
    post404 = status status404 >> text "Post not found"
    with400 a = rescue a (\msg -> status status400 >> text msg)
    postPost idParent = do
      maybePost <- liftIO =<< makePost <$> idParent <*> param "by" <*> param "content"
      maybe post404 json maybePost
    makePost idParent by content = do
      newPost <- createPost db by content idParent
      whenMaybe newPost broadcast
      return newPost
    whenMaybe = flip (maybe (return ()))

main :: IO ()
main = do
  conf <- Conf.require <$> Conf.load [Conf.Required "conf"]
  port <- conf "port"
  database <- newDatabase
  (broadcast, server) <- Sockets.newServer
  api <- app database broadcast
  putStrLn $ "Running on port " ++ show port
  Warp.run port (websocketsOr defaultConnectionOptions server api)
