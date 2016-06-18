module Database.Users (
  createCode,
  createToken,
  getUser,
  createUser,
  getUserByToken
) where

import ClassyPrelude
import Data.Maybe (fromJust)
import Database.Internal
import Data.Time.Clock (diffUTCTime)
import Types

toUser :: [SqlValue] -> User
toUser [idUser, name, email] =
  User { userID = fromSql idUser
       , userName = fromSql name
       , userEmail = fromSql email
       }

getUserByEmail :: EmailAddress -> DatabaseM (Maybe User)
getUserByEmail email =
  listToMaybe . fmap toUser <$> runQuery query [toSql email]
  where query = "select * from users where email = ?"

getUser :: ID -> DatabaseM (Maybe User)
getUser idUser =
  listToMaybe . fmap toUser <$> runQuery query [toSql idUser]
  where query = "select * from users where id = ?"

getUserByToken :: Token -> DatabaseM (Maybe User)
getUserByToken token = listToMaybe . fmap toUser <$> runQuery query [toSql token]
  where
    query = unlines [ "select users.* from tokens"
                    , "inner join users on tokens.id_user = users.id"
                    , "where token = ?"
                    ]

saneSql :: Bool -> SqlValue
saneSql True = toSql (1 :: Int)
saneSql False = toSql (0 :: Int)

insertCode :: CodeRecord -> DatabaseM ID
insertCode CodeRecord{..} =
  fromJust <$> insertRow query args
  where
    query = unlines [ "insert into codes"
                    , "(code, generated_at, valid, id_user)"
                    , "values (?, ?, ?, ?)"
                    ]
    args = [ toSql codeValue, toSql codeGeneratedAt
           , saneSql codeValid, toSql codeUserID ]

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
      db <- ask
      now <- liftIO getCurrentTime
      codeValue <- liftIO (secureRandom db)
      let code = CodeRecord { codeValue = codeValue
                            , codeGeneratedAt = now
                            , codeValid = True
                            , codeUserID = userID user
                            }
      insertCode code
      pure code

isCodeValidAt :: CodeRecord -> UTCTime -> Bool
isCodeValidAt CodeRecord{codeValid = False} _ = False
isCodeValidAt CodeRecord{codeValid = True, codeGeneratedAt} at =
  diffUTCTime at codeGeneratedAt < oneHour
  where oneHour = 60 * 60

toCodeRecord :: [SqlValue] -> CodeRecord
toCodeRecord [code, at, valid, idUser] =
  CodeRecord { codeValue = fromSql code
             , codeGeneratedAt = fromSql at
             , codeValid = fromSql valid
             , codeUserID = fromSql idUser
             }

findCode :: Code -> DatabaseM (Maybe CodeRecord)
findCode code =
  listToMaybe . fmap toCodeRecord <$> runQuery query [toSql code]
  where query = "select * from codes where code = ?"

insertToken :: Connection -> Text -> CodeRecord -> IO TokenRecord
insertToken conn token CodeRecord{..} = do
  idToken <- fromJust <$> insertRowRaw conn query [toSql token, toSql codeUserID]
  pure TokenRecord { tokenID = idToken
                   , tokenValue = token
                   , tokenUserID = codeUserID
                   }
  where
    query = "insert into tokens (token, id_user) values (?, ?)"

invalidateCode :: Connection -> CodeRecord -> IO ()
invalidateCode conn CodeRecord{codeValue} = do
  rowCount <- run conn query args
  assert (rowCount == 1) pure ()
  where
    query = "update codes set valid = 0 where code = ?"
    args = [toSql codeValue]

convertCodeToToken :: CodeRecord -> DatabaseM TokenRecord
convertCodeToToken code = do
  db <- ask
  liftIO $ withTransaction (dbConn db) $ \conn -> do
    invalidateCode conn code
    token <- secureRandom db
    insertToken conn token code

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
  idUserMaybe <- insertRow query [toSql email, toSql name]
  case idUserMaybe of
    Nothing -> pure Nothing
    Just idUser -> pure $ Just User { userID = idUser
                                    , userEmail = email
                                    , userName = name
                                    }
  where
    query = "insert into users (email, name) values (?, ?)"
