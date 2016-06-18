module Main (main) where

import           ClassyPrelude
import           Control.Concurrent.Lifted
import           Data.CaseInsensitive (original)
import           Data.Char (isAlphaNum)
import qualified Data.Configurator as Conf
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
maybeParam name = (Just <$> param name) `rescue` (pure . const Nothing)

defaultParam :: Parsable a => Lazy.Text -> a -> ActionM a
defaultParam name def = param name `rescue` (pure . const def)

validated :: Parsable a => (a -> Bool) -> Lazy.Text -> ActionM a -> ActionM a
validated f errorMessage val = do
  inner <- val
  if f inner then
    val
  else
    raise errorMessage

maybeHeader :: HeaderName -> ActionM (Maybe Text)
maybeHeader name = (listToMaybe . map (decodeUtf8 . snd)
                   . filter ((== name) . fst) . Wai.requestHeaders) <$> request

getHeader :: HeaderName -> ActionM Text
getHeader name = maybe (raise message) pure =<< maybeHeader name
  where message = "missing \"" <> headerName <> "\" header"
        headerName = (fromStrict . decodeUtf8 . original) name

route :: Database
      -> Chan (EmailAddress, Code)
      -> Chan ResolvedPost
      -> (ActionM () -> ScottyM ())
      -> ActionM (Either Response Request)
      -> ScottyM ()
route db emailChan socketChan path makeReq = path $ do
  reqOrRes <- makeReq
  let dbRes = either pure execute reqOrRes
  res <- liftIO (runReaderT dbRes db)
  effects <- send res
  mapM_ (liftIO . perform) effects
  where
    perform :: SideEffect -> IO ()
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
  (Right <$> makeReq) `rescue` (pure . Left . BadRequest)

execute :: Request -> DatabaseM Response
execute (GetPost idPost) = maybe (PostNotFound idPost) ExistingPost <$> getPost idPost
execute (ListPosts query) = PostList <$> getPosts query
execute (CreatePost idParent token content) =
  maybe (pure BadToken) makePost =<< getUserByToken token
  where makePost user = do
          maybePost <- createPost user content idParent
          pure $ case (idParent, maybePost) of
            (_, Just post) -> NewPost post
            (Just idParent, Nothing) -> PostNotFound idParent
            (Nothing, Nothing) -> error "creating top-level posts should never fail"
execute (CreateCode emailAddress) =
  maybe UnknownEmail NewCode <$> createCode emailAddress
execute (CreateToken code) =
  maybe (pure BadCode) (fmap NewToken . withUser) =<< createToken code
  where
    noUser = error "we created a token but then couldn't find the user"
    withUser token@TokenRecord{tokenUserID} =
      maybe noUser (ResolvedToken token) <$> getUser tokenUserID
execute (CreateUser email name) =
  maybe (pure ExistingNameOrEmail) withCode =<< createUser email name
  where withCode User{userEmail} = maybe noCode NewUser <$> createCode userEmail
        noCode = error "failed to create code for new user"

data SideEffect = SendEmail EmailAddress Code
                | SocketUpdate ResolvedPost

done :: ActionM [SideEffect]
done = pure []

send :: Response -> ActionM [SideEffect]
send (NewPost p) = json p >> pure [SocketUpdate p]
send (ExistingPost p) = json p >> done
send (PostList ps) = json ps >> done
send (NewToken t) = json t >> done
send (NewUser resolvedCode@(ResolvedCode _ user)) = json user >> send (NewCode resolvedCode)
send (NewCode (ResolvedCode code user)) = pure [SendEmail (userEmail user)
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

basilica :: Maybe ByteString
         -> Database
         -> Chan (EmailAddress, Code)
         -> Chan ResolvedPost
         -> IO Application
basilica origin db emailChan socketChan = scottyApp $ do
  case origin of
    Nothing -> pure ()
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
      pure (Left InvalidUsername)

isValidName :: Text -> Bool
isValidName name = all isAlphaNum name && (len >= 2) && (len < 20)
  where len = length name

addHeaders :: ResponseHeaders -> Wai.Middleware
addHeaders newHeaders app req respond = app req $ \response -> do
  let (st, currentHeaders, streamHandle) = Wai.responseToStream response
  streamHandle $ \streamBody ->
    respond $ Wai.responseStream st (currentHeaders ++ newHeaders) streamBody

randomSubject :: IO Text
randomSubject = (subjects `indexEx`) <$> getStdRandom (randomR (0, length subjects - 1))
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

sendCodeMail :: Mailer -> Text -> (EmailAddress, Code) -> IO ()
sendCodeMail mailer clientUrl (to, code) = do
  subject <- randomSubject
  sendMail mailer (easyEmail to subject messageBody)
  where messageBody = intercalate "\n"
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
logCode (to, code) = putStrLn (intercalate ": " [to, code])

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "conf"]
  port <- Conf.require conf "port"
  origin <- Conf.lookup conf "client-origin"
  mailgunKey <- Conf.lookup conf "mailgun-key"
  mailHandler <- case mailgunKey of
    Nothing -> pure logCode
    Just key -> sendCodeMail (newMailer key) <$> Conf.require conf "client-url"
  db <- newDatabase =<< Conf.require conf "dbpath"
  emailChan <- newChan
  socketChan <- newChan
  server <- Sockets.newServer socketChan
  api <- basilica origin db emailChan socketChan
  _ <- fork $ getChanContents emailChan >>= mapM_ mailHandler
  putStrLn $ "Running on port " ++ tshow port
  Warp.run port (websocketsOr defaultConnectionOptions server api)
