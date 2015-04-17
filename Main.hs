module Main (basilicaWaiApp, loadConfig, main) where

import           BasePrelude hiding (app, intercalate)
import           Config
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Trans (liftIO)
import           Data.ByteString (ByteString)
import           Data.CaseInsensitive (original)
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

defaultParam :: Parsable a => Lazy.Text -> a -> ActionM a
defaultParam name def = (param name) `rescue` (return . const def)

validated :: Parsable a => (a -> Bool) -> Lazy.Text -> ActionM a -> ActionM a
validated f errorMessage val = do
  inner <- val
  if f inner then
    val
  else
    raise errorMessage

maybeHeader :: HeaderName -> ActionM (Maybe Strict.Text)
maybeHeader name = (listToMaybe . map (Strict.decodeUtf8 . snd)
                   . filter ((== name) . fst) . Wai.requestHeaders) <$> request

getHeader :: HeaderName -> ActionM Strict.Text
getHeader name = maybe (raise message) return =<< maybeHeader name
  where message = "missing \"" <> headerName <> "\" header"
        headerName = (Lazy.fromStrict . Strict.decodeUtf8 . original) name

route :: Database
      -> Chan (EmailAddress, Code)
      -> Chan ResolvedPost
      -> (ActionM () -> ScottyM ())
      -> ActionM (Either Response Request)
      -> ScottyM ()
route db emailChan socketChan path makeReq = path $ do
  reqOrRes <- makeReq
  let dbRes = either return execute reqOrRes
  res <- liftIO (runReaderT dbRes db)
  effects <- send res
  mapM_ (liftIO . perform) effects
  where
    perform (SendEmail emailAddress code) =
      writeChan emailChan (emailAddress, code)
    perform (SocketUpdate p) =
      writeChan socketChan p

simpleRoute :: Database
            -> Chan (EmailAddress, Code)
            -> Chan ResolvedPost
            -> (ActionM () -> ScottyM ())
            -> ActionM Request -> ScottyM ()
simpleRoute db emailChan socketChan path makeReq = route db emailChan socketChan path $
  (Right <$> makeReq) `rescue` (return . Left . BadRequest)
 
execute :: Request -> DatabaseM Response
execute (GetPost idPost) = maybe (PostNotFound idPost) ExistingPost <$> getPost idPost
execute (ListPosts query) = PostList <$> getPosts query
execute (CreatePost idParent token content) = do
  maybeUser <- getUserByToken token
  case maybeUser of
    Nothing -> return BadToken
    Just user -> maybe (PostNotFound $ fromJust idParent) NewPost
                 <$> createPost user content idParent
execute (CreateCode emailAddress) =
  maybe UnknownEmail NewCode <$> createCode emailAddress
execute (CreateToken code) =
  maybe (return BadCode) (fmap NewToken . withUser) =<< createToken code
  where withUser token@TokenRecord{tokenUserID} =
          (token,) . fromJust <$> getUser tokenUserID
execute (CreateUser email name) = do
  maybeUser <- createUser email name
  case maybeUser of
    Nothing -> return ExistingNameOrEmail
    Just User{userEmail} -> NewUser . fromJust <$> createCode userEmail

data SideEffect = SendEmail EmailAddress Code
                | SocketUpdate ResolvedPost

done :: ActionM [SideEffect]
done = return []

send :: Response -> ActionM [SideEffect]
send (NewPost p) = json p >> return [SocketUpdate p]
send (ExistingPost p) = json p >> done
send (PostList ps) = json ps >> done
send (NewToken t) = json t >> done
send (NewUser resolvedCode@(_, user)) = json user >> send (NewCode resolvedCode)
send (NewCode (code, user)) = return [SendEmail (userEmail user)
                                                (codeValue code)]
send BadToken = status status401 >> text "invalid token" >> done
send BadCode = status status401 >> text "invalid code" >> done
send UnknownEmail = status status200 >> done
send InvalidUsername = status status400 >> text "invalid username" >> done
send ExistingNameOrEmail = status status409 >> text "username or email address taken" >> done
send (BadRequest message) = status status400 >> text message >> done
send (PostNotFound idPost) = status status404 >> text message >> done
  where message = mconcat ["post ", (Lazy.pack . show) idPost, " not found"]

isLegalLimit :: Int -> Bool
isLegalLimit x
  | x < 1 = False
  | x > 500 = False
  | otherwise = True

basilicaHttpHandler :: Maybe ByteString
                    -> Database
                    -> Chan (EmailAddress, Code)
                    -> Chan ResolvedPost
                    -> ScottyM ()
basilicaHttpHandler origin db emailChan socketChan = do
  case origin of
    Nothing -> return ()
    Just o -> do
      middleware (addHeaders [("Access-Control-Allow-Origin", o)])
      addroute OPTIONS (function $ const $ Just []) $ do
        setHeader "Access-Control-Allow-Headers" "X-Token"
        setHeader "Access-Control-Allow-Methods" "GET, POST, PUT, PATCH, DELETE, OPTIONS"
        status status200

  let simple = simpleRoute db emailChan socketChan

  let limit = validated isLegalLimit "limit out of range"
                        (defaultParam "limit" 200)
  simple (get "/posts") (ListPosts <$> (PostQuery <$> maybeParam "before"
                                                  <*> maybeParam "after"
                                                  <*> limit))
  simple (get "/posts/:id") (GetPost <$> param "id")
  simple (post "/posts") (CreatePost Nothing <$> getHeader "X-Token"
                                             <*> param "content")
  simple (post "/posts/:id") (CreatePost <$> (Just <$> param "id")
                                         <*> getHeader "X-Token"
                                         <*> param "content")
  simple (post "/codes") (CreateCode <$> param "email")
  simple (post "/tokens") (CreateToken <$> param "code")

  route db emailChan socketChan (post "/users") $ do
    name <- param "name"
    if isValidName name then
      (fmap Right . CreateUser) <$> param "email" <*> pure name
    else
      return (Left InvalidUsername)

isValidName :: Strict.Text -> Bool
isValidName name =
  all isAlphaNum (Strict.unpack name)
  && (len >= 2) && (len < 20)
  where len = Strict.length name

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

basilicaWaiApp :: Config -> IO Application
basilicaWaiApp Config {..} = do
  let mailHandler = case confMandrillKey of
        Nothing -> logCode
        Just key -> sendCodeMail (newMailer key) confClientUrl
  db <- newDatabase confDBPath
  emailChan <- newChan
  socketChan <- newChan
  wsServer <- Sockets.newServer socketChan
  httpServer <- scottyApp $ basilicaHttpHandler confClientOrigin db emailChan socketChan
  forkIO $ getChanContents emailChan >>= mapM_ mailHandler
  return $ websocketsOr defaultConnectionOptions wsServer httpServer

main :: IO ()
main = do
  config <- loadConfig
  app <- basilicaWaiApp config
  putStrLn $ "Running on port " ++ show (confPort config)
  Warp.run (confPort config) app
