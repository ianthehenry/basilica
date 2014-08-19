module Main where

import           BasePrelude hiding (app)
import           Control.Monad.Trans (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.Configurator as Conf
import           Data.Text.Lazy (Text)
import           Database
import           Network.HTTP.Types
import           Network.Wai (Application)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.WebSockets.Connection (defaultConnectionOptions)
import qualified Sockets
import           Web.Scotty

maybeParam :: Parsable a => Text -> ActionM (Maybe a)
maybeParam name = (Just <$> param name) `rescue` (return . const Nothing)

basilica :: Maybe ByteString -> Database -> IO Application
basilica origin db = scottyApp $ do
  case origin of
    Nothing -> return ()
    Just o -> middleware (addHeaders [("Access-Control-Allow-Origin", o)])
  get "/posts/:id" $
    withPost json =<< param "id"
  get "/posts" $ do
    since <- maybeParam "after"
    liftIO (getPostsSince db since) >>= json
  post "/posts" $
    with400 $ postPost (return Nothing)
  post "/posts/:id" $
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
    makePost idParent by content = createPost db by content idParent

addHeaders :: ResponseHeaders -> Wai.Middleware
addHeaders newHeaders app req respond = app req $ \response -> do
  let (st, currentHeaders, streamHandle) = Wai.responseToStream response
  streamHandle $ \streamBody ->
    respond $ Wai.responseStream st (currentHeaders ++ newHeaders) streamBody

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "conf"]
  port <- Conf.require conf "port"
  origin <- Conf.lookup conf "client-origin"
  db@(_, newPosts) <- newDatabase =<< Conf.require conf "dbpath"
  server <- Sockets.newServer newPosts
  api <- basilica origin db
  putStrLn $ "Running on port " ++ show port
  Warp.run port (websocketsOr defaultConnectionOptions server api)
