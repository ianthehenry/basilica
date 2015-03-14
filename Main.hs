module Main where

import           BasePrelude hiding (app, intercalate)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Reader (runReaderT)
import           Data.ByteString (ByteString)
import qualified Data.Configurator as Conf
import qualified Data.Text as Strict
import qualified Data.Text.Encoding as Strict
import qualified Data.Text.IO as Text
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
import           Routes

maybeParam :: Parsable a => Lazy.Text -> ActionM (Maybe a)
maybeParam name = (Just <$> param name) `rescue` (return . const Nothing)

getHeader :: HeaderName -> ActionM (Maybe Strict.Text)
getHeader name = (listToMaybe . map (Strict.decodeUtf8 . snd)
                  . filter ((== name) . fst) . Wai.requestHeaders) <$> request

maybDB :: (a -> DatabaseM (Maybe b)) -> Maybe a -> DatabaseM (Maybe b)
maybDB = maybe (return Nothing)

lmdb :: MonadIO m => Database -> (a -> DatabaseM (Maybe b)) -> Maybe a -> m (Maybe b)
lmdb db f = liftDB db . maybDB f

liftDB :: MonadIO m => Database -> DatabaseM a -> m a
liftDB db inner = liftIO (runReaderT inner db)

route :: Database -> (ActionM () -> ScottyM ()) -> ActionM (Either Response Request) -> ScottyM ()
route db path makeReq = path $ do
  reqOrRes <- makeReq
  let dbRes = either return execute reqOrRes
  res <- liftDB db dbRes
  send res

simpleRoute :: Database -> (ActionM () -> ScottyM ()) -> ActionM Request -> ScottyM ()
simpleRoute db path makeReq = route db path $
  (Right <$> makeReq) `rescue` (return . Left . BadRequest)
 
execute :: Request -> DatabaseM Response
execute (GetPost idPost) = maybe (PostNotFound idPost) ExistingPost <$> getPost idPost

send :: Response -> ActionM ()
send (PostNotFound idPost) = status status404 >> text message
  where message = mconcat ["post ", (Lazy.pack . show) idPost, " not found"]
send (BadRequest message) = status status400 >> text message
send (ExistingPost p) = json p

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

  simpleRoute db (get "/posts/:id") (GetPost <$> param "id")

  get "/posts" $ do
    since <- maybeParam "after"
    liftDB db (getPostsSince since) >>= json
  post "/posts" $ with400 $ postPost Nothing
  post "/posts/:id" $
    with400 $ (Just <$> param "id") >>= postPost
  post "/codes" $
    with400 $ do
      emailAddress <- param "email"
      liftDB db $ do
        code <- createCode emailAddress
        maybe (return ()) (liftIO . writeChan emailChan . (emailAddress,)) code
      status status200
  post "/tokens" $
    with400 $ do
      code <- param "code"
      token <- liftDB db (createToken code)
      maybe (status status401) (\t -> liftDB db (withUser t) >>= json) token
  post "/users" $
    with400 $ do
      email <- param "email"
      name <- param "name"
      if isValidName name then do
        user <- liftDB db (createUser email name)
        when (isJust user) $ liftDB db $ do
           code <- fromJust <$> createCode email
           liftIO $ writeChan emailChan (email, code)
        maybe (status status409) json user
      else
        status status400 >> text "invalid username"
  where
    post404 idPost = status status404 >> text (postMessage idPost)
    postMessage idPost = mconcat ["post ", (Lazy.pack . show) idPost, " not found"]
    with400 a = rescue a (\msg -> status status400 >> text msg)
    isValidName name = all isAlphaNum (Strict.unpack name)
                       && (len >= 2) && (len < 20)
      where len = Strict.length name
    postPost idParent = do
      content <- param "content"
      maybeUser <- lmdb db getUserByToken =<< getHeader "X-Token"
      case maybeUser of
        Nothing -> status status401 >> text "invalid token"
        Just user -> do
          maybePost <- liftDB db (createPost user content idParent)
          maybe (post404 idParent) json maybePost

withUser :: TokenRecord -> DatabaseM (TokenRecord, User)
withUser token@TokenRecord{..} = do
  user <- fromJust <$> getUser tokenUserID
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

logCode :: (EmailAddress, CodeRecord) -> IO ()
logCode (to, CodeRecord{codeValue}) = Text.putStrLn (Strict.intercalate ": " [to, codeValue])

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "conf"]
  port <- Conf.require conf "port"
  origin <- Conf.lookup conf "client-origin"
  mandrillKey <- Conf.lookup conf "mandrill-key"
  mailHandler <- case mandrillKey of
    Nothing -> return logCode
    Just key -> sendCodeMail (newMailer key) <$> Conf.require conf "client-url"
  db <- newDatabase =<< Conf.require conf "dbpath"
  emailChan <- newChan
  server <- Sockets.newServer (dbPostChan db)
  api <- basilica origin db emailChan
  forkIO $ getChanContents emailChan >>= mapM_ mailHandler
  putStrLn $ "Running on port " ++ show port
  Warp.run port (websocketsOr defaultConnectionOptions server api)
