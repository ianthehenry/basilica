module Main where

import           BasePrelude hiding (app, intercalate)
import           Control.Concurrent.Chan
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.Configurator as Conf
import qualified Data.Text as Strict
import qualified Data.Text.Encoding as Strict
import qualified Data.Text.Lazy as Lazy
import           Database
import           Mailer
import           Network.HTTP.Types
import           Network.Wai (Application)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.WebSockets.Connection (defaultConnectionOptions)
import qualified Sockets
import           System.Random (getStdRandom, randomR)
import           Web.Scotty


maybeParam :: Parsable a => Lazy.Text -> ActionM (Maybe a)
maybeParam name = (Just <$> param name) `rescue` (return . const Nothing)

getHeader :: HeaderName -> ActionM (Maybe Strict.Text)
getHeader name = (listToMaybe . map (Strict.decodeUtf8 . snd)
                  . filter ((== name) . fst) . Wai.requestHeaders) <$> request

maybio :: (a -> IO (Maybe b)) -> Maybe a -> IO (Maybe b)
maybio = maybe (return Nothing)

lmio :: MonadIO m => (a -> IO (Maybe b)) -> Maybe a -> m (Maybe b)
lmio f = liftIO . maybio f

basilica :: Maybe ByteString -> Database -> Chan (EmailAddress, CodeRecord) -> IO Application
basilica origin db emailChan = scottyApp $ do
  case origin of
    Nothing -> return ()
    Just o -> do
      middleware (addHeaders [("Access-Control-Allow-Origin", o)])
      addroute OPTIONS (function $ const $ Just []) $ do
        setHeader "Access-Control-Allow-Headers" "X-Token"
        setHeader "Access-Control-Allow-Methods" "GET, POST, PUT, PATCH, DELETE, OPTIONS"
        status status200
  get "/posts/:id" $
    withPost json =<< param "id"
  get "/posts" $ do
    since <- maybeParam "after"
    liftIO (getPostsSince db since) >>= json
  post "/posts" $ with400 $ postPost Nothing
  post "/posts/:id" $
    with400 $ (Just <$> param "id") >>= postPost
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
  post "/users" $
    with400 $ do
      email <- param "email"
      name <- param "name"
      if isValidName name then do
        user <- liftIO (createUser db email name)
        when (isJust user) $ liftIO $ do
           code <- fromJust <$> createCode db email
           writeChan emailChan (email, code)
        maybe (status status409) json user
      else
        status status400 >> text "invalid username"
  where
    withPost f idPost = do
      liftIO $ print idPost
      maybe (post404 idPost) f =<< liftIO (getPost db idPost)
    post404 idPost = status status404 >> text (postMessage idPost)
    postMessage idPost = mconcat ["post ", (Lazy.pack . show) idPost, " not found"]
    with400 a = rescue a (\msg -> status status400 >> text msg)
    isValidName name = all isAlphaNum (Strict.unpack name)
                       && (len >= 2) && (len < 20)
      where len = Strict.length name
    postPost idParent = do
      content <- param "content"
      maybeUser <- lmio (getUserByToken db) =<< getHeader "X-Token"
      case maybeUser of
        Nothing -> status status401 >> text "invalid token"
        Just user -> do
          maybePost <- liftIO (createPost db user content idParent)
          maybe (post404 idParent) json maybePost

withUser :: Database -> TokenRecord -> IO (TokenRecord, User)
withUser db token@TokenRecord{..} = do
  user <- fromJust <$> getUser db tokenUserID
  return (token, user)

addHeaders :: ResponseHeaders -> Wai.Middleware
addHeaders newHeaders app req respond = app req $ \response -> do
  let (st, currentHeaders, streamHandle) = Wai.responseToStream response
  streamHandle $ \streamBody ->
    respond $ Wai.responseStream st (currentHeaders ++ newHeaders) streamBody

randomSubject :: IO Strict.Text
randomSubject = (subjects !!) <$> getStdRandom (randomR (0, length subjects - 1))
  where subjects = [ "Hey Beautiful"
                   , "Hey Baby"
                   , "Hey Hon"
                   , "Hey Honey"
                   , "Hey Girl"
                   , "Hey Sugarlips"
                   , "Hey Darling"
                   , "Hey Buttercup"
                   , "Hey Honeyfingers"
                   , "Hey Syruptoes"
                   ]

sendCodeMail :: Mailer -> Strict.Text -> (EmailAddress, CodeRecord) -> IO ()
sendCodeMail mailer clientUrl (to, CodeRecord{codeValue}) = do
  subject <- randomSubject
  sendMail mailer (easyEmail to subject messageBody)
  where messageBody = Strict.intercalate "\n"
                      [ "Here's your Basilicode:"
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
