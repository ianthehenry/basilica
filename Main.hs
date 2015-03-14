module Main where

import           BasePrelude hiding (app, intercalate)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Trans (liftIO)
import           Data.ByteString (ByteString)
import           Data.CaseInsensitive (original)
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
import           Routes
import qualified Sockets
import           System.Random (getStdRandom, randomR)
import           Web.Scotty

maybeParam :: Parsable a => Lazy.Text -> ActionM (Maybe a)
maybeParam name = (Just <$> param name) `rescue` (return . const Nothing)

maybeHeader :: HeaderName -> ActionM (Maybe Strict.Text)
maybeHeader name = (listToMaybe . map (Strict.decodeUtf8 . snd)
                   . filter ((== name) . fst) . Wai.requestHeaders) <$> request

getHeader :: HeaderName -> ActionM Strict.Text
getHeader name = maybe (raise message) return =<< maybeHeader name
  where message = "missing \"" <> headerName <> "\" header"
        headerName = (Lazy.fromStrict . Strict.decodeUtf8 . original) name

maybDB :: (a -> DatabaseM (Maybe b)) -> Maybe a -> DatabaseM (Maybe b)
maybDB = maybe (return Nothing)

lmdb :: MonadIO m => Database -> (a -> DatabaseM (Maybe b)) -> Maybe a -> m (Maybe b)
lmdb db f = liftDB db . maybDB f

liftDB :: MonadIO m => Database -> DatabaseM a -> m a
liftDB db inner = liftIO (runReaderT inner db)

route :: Database -> Chan (EmailAddress, Code)
                  -> (ActionM () -> ScottyM ())
                  -> ActionM (Either Response Request)
                  -> ScottyM ()
route db emailChan path makeReq = path $ do
  reqOrRes <- makeReq
  let dbRes = either return execute reqOrRes
  res <- liftDB db dbRes
  effect <- send res
  case effect of
    Noop -> return ()
    SendEmail emailAddress code ->
      liftIO (writeChan emailChan (emailAddress, code))

simpleRoute :: Database -> Chan (EmailAddress, Code)
                        -> (ActionM () -> ScottyM ())
                        -> ActionM Request -> ScottyM ()
simpleRoute db emailChan path makeReq = route db emailChan path $
  (Right <$> makeReq) `rescue` (return . Left . BadRequest)
 
execute :: Request -> DatabaseM Response
execute (GetPost idPost) = maybe (PostNotFound idPost) ExistingPost <$> getPost idPost
execute (ListPosts since) = PostList <$> getPostsSince since
execute (CreatePost idParent token content) = do
  maybeUser <- getUserByToken token
  case maybeUser of
    Nothing -> return BadToken
    Just user -> maybe (PostNotFound $ fromJust idParent) NewPost
                 <$> createPost user content idParent
execute (CreateCode emailAddress) =
  maybe UnknownEmail NewCode <$> createCode emailAddress

data SideEffect = Noop
                | SendEmail EmailAddress Code

done :: ActionM SideEffect
done = return Noop

send :: Response -> ActionM SideEffect
send (PostNotFound idPost) = status status404 >> text message >> done
  where message = mconcat ["post ", (Lazy.pack . show) idPost, " not found"]
send (BadRequest message) = status status400 >> text message >> done
send (ExistingPost p) = json p >> done
send (NewPost p) = json p >> done
send (PostList ps) = json ps >> done
send (NewCode (code, user)) = status status200 >> return (SendEmail (userEmail user) (codeValue code))
send BadToken = status status401 >> text "invalid token" >> done
send UnknownEmail = status status200 >> done

basilica :: Maybe ByteString -> Database -> Chan (EmailAddress, Code) -> IO Application
basilica origin db emailChan = scottyApp $ do
  case origin of
    Nothing -> return ()
    Just o -> do
      middleware (addHeaders [("Access-Control-Allow-Origin", o)])
      addroute OPTIONS (function $ const $ Just []) $ do
        setHeader "Access-Control-Allow-Headers" "X-Token"
        setHeader "Access-Control-Allow-Methods" "GET, POST, PUT, PATCH, DELETE, OPTIONS"
        status status200

  let simple = simpleRoute db emailChan

  simple (get "/posts/:id") (GetPost <$> param "id")
  simple (get "/posts") (ListPosts <$> maybeParam "after")
  simple (post "/posts") (CreatePost Nothing <$> getHeader "X-Token"
                                             <*> param "content")
  simple (post "/posts/:id") (CreatePost <$> (Just <$> param "id")
                                         <*> getHeader "X-Token"
                                         <*> param "content")
  simple (post "/codes") (CreateCode <$> param "email")

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
           (CodeRecord{codeValue}, User{userEmail}) <- fromJust <$> createCode email
           liftIO $ writeChan emailChan (userEmail, codeValue)
        maybe (status status409) json user
      else
        status status400 >> text "invalid username"
  where
    with400 a = rescue a (\msg -> status status400 >> text msg)
    isValidName name = all isAlphaNum (Strict.unpack name)
                       && (len >= 2) && (len < 20)
      where len = Strict.length name

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

sendCodeMail :: Mailer -> Strict.Text -> (EmailAddress, Code) -> IO ()
sendCodeMail mailer clientUrl (to, code) = do
  subject <- randomSubject
  sendMail mailer (easyEmail to subject messageBody)
  where messageBody = Strict.intercalate "\n"
                      [ "Here's your Basilicode:"
                      , ""
                      , code
                      , ""
                      , "And a handy login link:"
                      , ""
                      , clientUrl <> "/login?code=" <> code
                      , ""
                      , "Love,"
                      , "  Basilica"
                      ]

logCode :: (EmailAddress, Code) -> IO ()
logCode (to, code) = Text.putStrLn (Strict.intercalate ": " [to, code])

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
