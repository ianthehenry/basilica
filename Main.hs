module Main where

import           BasePrelude hiding (app, intercalate)
import           Control.Concurrent.Chan
import           Control.Monad.Trans (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.Configurator as Conf
import           Data.Text as Strict
import           Data.Text.Lazy as Lazy
import           Database
import           Mailer
import           Network.HTTP.Types
import           Network.Wai (Application)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.WebSockets.Connection (defaultConnectionOptions)
import qualified Sockets
import           Web.Scotty

maybeParam :: Parsable a => Lazy.Text -> ActionM (Maybe a)
maybeParam name = (Just <$> param name) `rescue` (return . const Nothing)

basilica :: Maybe ByteString -> Database -> Chan (EmailAddress, CodeRecord) -> IO Application
basilica origin db emailChan = scottyApp $ do
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
  post "/codes" $
    with400 $ do
      emailAddress <- param "email"
      liftIO $ do
        code <- createCode db emailAddress
        maybe (return ()) (writeChan emailChan . (emailAddress, )) code
      status status200
  post "/tokens" $
    with400 $ do
      code <- param "code"
      token <- liftIO (createToken db code)
      maybe (status status401) (\t -> liftIO (withUser db t) >>= json) token
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

withUser :: Database -> TokenRecord -> IO (TokenRecord, User)
withUser db token@TokenRecord{..} = do
  user <- fromJust <$> getUser db tokenUserID
  return (token, user)

addHeaders :: ResponseHeaders -> Wai.Middleware
addHeaders newHeaders app req respond = app req $ \response -> do
  let (st, currentHeaders, streamHandle) = Wai.responseToStream response
  streamHandle $ \streamBody ->
    respond $ Wai.responseStream st (currentHeaders ++ newHeaders) streamBody

sendCodeMail :: Mailer -> Strict.Text -> (EmailAddress, CodeRecord) -> IO ()
sendCodeMail mailer clientUrl (to, CodeRecord{codeValue}) =
  sendMail mailer (easyEmail to "Basilica Code" messageBody)
  where messageBody = Strict.intercalate "\n"
                      [ "Here's your code:"
                      , ""
                      , codeValue
                      , ""
                      , "And a handy login link:"
                      , ""
                      , clientUrl <> "/login?code=" <> codeValue
                      , ""
                      , "Love,"
                      , "  Basilica"
                      ]

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "conf"]
  port <- Conf.require conf "port"
  origin <- Conf.lookup conf "client-origin"
  mailer <- newMailer <$> Conf.require conf "mandrill-key"
  clientUrl <- Conf.require conf "client-url"
  db <- newDatabase =<< Conf.require conf "dbpath"
  emailChan <- newChan
  server <- Sockets.newServer (dbPostChan db)
  api <- basilica origin db emailChan
  forkIO $ getChanContents emailChan >>= mapM_ (sendCodeMail mailer clientUrl)
  putStrLn $ "Running on port " ++ show port
  Warp.run port (websocketsOr defaultConnectionOptions server api)
