module Database.Users
( createCode
, createToken
, getUser
, createUser
, getUserByToken
) where

import ClassyPrelude hiding (on)
import Control.Monad.Reader (asks)
import Data.Time.Clock (diffUTCTime)
import Database.Esqueleto hiding (Connection)
import Database.Internal
import Database.Schema

toUser :: Entity UserRow -> User
toUser userEntity = User { userID = getID userEntity
                         , userName = userRowName userRow
                         , userEmail = userRowEmail userRow
                         }
  where userRow = entityVal userEntity

getUserByEmail :: EmailAddress -> DatabaseM (Maybe User)
getUserByEmail email = getOne toUser query
  where query = select $ from $ \user -> do
          where_ $ user ^. UserRowEmail ==. val email
          pure user

getUser :: ID -> DatabaseM (Maybe User)
getUser idUser = getOne toUser query
  where query = select $ from $ \user -> do
          where_ $ user ^. UserRowId ==. val (fromInt idUser)
          pure user

getUserByToken :: Token -> DatabaseM (Maybe User)
getUserByToken x = getOne toUser query
  where query = select $ from $ \(token `InnerJoin` user) -> do
          on $ (token ^. TokenRowUserId) ==. (user ^. UserRowId)
          where_ $ token ^. TokenRowToken ==. val x
          pure user

insertCode :: CodeRecord -> DatabaseM ID
insertCode CodeRecord{..} = asInt <$> runQuery query
  where
    query = insert codeRow
    codeRow = CodeRow { codeRowCode = codeValue
                      , codeRowGeneratedAt = codeGeneratedAt
                      , codeRowValid = codeValid
                      , codeRowUserId = fromInt codeUserID
                      }

createCode :: EmailAddress -> DatabaseM (Maybe ResolvedCode)
createCode email = do
  maybeUser <- getUserByEmail email
  case maybeUser of
    Nothing -> pure Nothing
    Just user -> do
      code <- newCode user
      pure (Just (ResolvedCode code user))
  where
    newCode user = do
      rng <- asks dbRNG
      now <- liftIO getCurrentTime
      codeValue <- liftIO (secureRandom rng)
      let code = CodeRecord { codeValue = codeValue
                            , codeGeneratedAt = now
                            , codeValid = True
                            , codeUserID = userID user
                            }
      _ <- insertCode code
      pure code

isCodeValidAt :: CodeRecord -> UTCTime -> Bool
isCodeValidAt CodeRecord{codeValid = False} _ = False
isCodeValidAt CodeRecord{codeValid = True, codeGeneratedAt} at =
  diffUTCTime at codeGeneratedAt < oneHour
  where oneHour = 60 * 60

toCodeRecord :: Entity CodeRow -> CodeRecord
toCodeRecord codeEntity =
  CodeRecord { codeValue = codeRowCode codeRow
             , codeGeneratedAt = codeRowGeneratedAt codeRow
             , codeValid = codeRowValid codeRow
             , codeUserID = asInt (codeRowUserId codeRow)
             }
  where codeRow = entityVal codeEntity

findCode :: Code -> DatabaseM (Maybe CodeRecord)
findCode x = getOne toCodeRecord query
  where query = select $ from $ \code -> do
          where_ $ code ^. CodeRowCode ==. val x
          pure code

insertToken :: MonadIO m => Text -> CodeRecord -> SqlPersistT m TokenRecord
insertToken token CodeRecord{..} = do
  idToken <- insert tokenRow
  pure TokenRecord { tokenID = asInt idToken
                   , tokenValue = token
                   , tokenUserID = codeUserID
                   }
  where tokenRow = TokenRow { tokenRowToken = token
                            , tokenRowUserId = fromInt codeUserID
                            }

invalidateCode :: MonadIO m => CodeRecord -> SqlPersistT m ()
invalidateCode CodeRecord{codeValue} = do
  rowCount <- updateCount $ \code -> do
    set code [CodeRowValid =. val False]
    where_ $ code ^. CodeRowCode ==. val codeValue
  assert (rowCount == 1) (pure ())

convertCodeToToken :: CodeRecord -> DatabaseM TokenRecord
convertCodeToToken code = do
  rng <- asks dbRNG
  token <- liftIO (secureRandom rng)
  runQuery (invalidateCode code *> insertToken token code)

createToken :: Code -> DatabaseM (Maybe TokenRecord)
createToken code = do
  maybeCode <- findCode code
  case maybeCode of
    Nothing -> pure Nothing
    Just record -> do
      now <- liftIO getCurrentTime
      if isCodeValidAt record now then
        Just <$> convertCodeToToken record
      else pure Nothing

createUser :: EmailAddress -> Text -> DatabaseM (Maybe User)
createUser email name = do
  idUserMaybe <- runInsert userRow
  case idUserMaybe of
    Nothing -> pure Nothing
    Just idUser -> pure $ Just User { userID = asInt idUser
                                    , userEmail = email
                                    , userName = name
                                    }
  where
    userRow = UserRow { userRowName = name
                      , userRowEmail = email
                      }
