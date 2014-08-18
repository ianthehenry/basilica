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

app :: Database -> IO Application
app db = scottyApp $ do
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
    makePost idParent by content = createPost db by content idParent

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "conf"]
  port <- Conf.require conf "port"
  db@(_, newPosts) <- newDatabase =<< Conf.require conf "dbpath"
  server <- Sockets.newServer newPosts
  api <- app db
  putStrLn $ "Running on port " ++ show port
  Warp.run port (websocketsOr defaultConnectionOptions server api)
