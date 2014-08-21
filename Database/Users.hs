module Database.Users (
  createCode,
  createToken,
  getUser
) where

import           BasePrelude
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock
import           Database.Internal

toUser :: [SqlValue] -> User
toUser [idUser, email] =
  User { userID = fromSql idUser
       , userEmail = fromSql email
       }

getUser :: Connection -> Email -> IO (Maybe User)
getUser conn email =
  listToMaybe <$> fmap toUser <$> quickQuery' conn query [toSql email]
  where query = "select * from users where email = ?"

insertCode :: Connection -> CodeRecord -> IO ID
insertCode conn CodeRecord{..} =
  fromJust <$> insertRow conn query args
  where
    query = unlines [ "insert into codes"
                    , "(code, generated_at, valid, id_user)"
                    , "values (?, ?, ?, ?)"
                    ]
    args = [ toSql codeValue, toSql codeGeneratedAt
           , toSql codeValid, toSql codeUserID ]

createCode :: Database -> Email -> IO (Maybe CodeRecord)
createCode db@(Database{dbConn}) email =
  getUser dbConn email >>= maybe (return Nothing) (fmap Just . newCode)
  where
    newCode user = do
      now <- getCurrentTime
      codeValue <- secureRandom db
      let code = CodeRecord { codeValue = codeValue
                            , codeGeneratedAt = now
                            , codeValid = True
                            , codeUserID = userID user
                            }
      insertCode dbConn code
      return code

isCodeValidAt :: CodeRecord -> UTCTime -> Bool
isCodeValidAt CodeRecord {codeValid = False} _ = False
isCodeValidAt CodeRecord {codeValid = True, codeGeneratedAt} at =
  diffUTCTime at codeGeneratedAt < oneHour
  where oneHour = 60 * 60

toCodeRecord :: [SqlValue] -> CodeRecord
toCodeRecord [code, at, valid, idUser] =
  CodeRecord { codeValue = fromSql code
             , codeGeneratedAt = fromSql at
             , codeValid = fromSql valid
             , codeUserID = fromSql idUser
             }

findCode :: Connection -> Code -> IO (Maybe CodeRecord)
findCode conn code =
  listToMaybe <$> fmap toCodeRecord <$> quickQuery' conn query [toSql code]
  where query = "select * from codes where code = ?"

insertToken :: Connection -> Text -> CodeRecord -> IO TokenRecord
insertToken conn token CodeRecord{..} = do
  idToken <- fromJust <$> insertRow conn query [toSql token, toSql codeUserID]
  return TokenRecord { tokenID = idToken
                     , tokenValue = token
                     , tokenUserID = codeUserID
                     }
  where
    query = "insert into tokens (token, id_user) values (?, ?)"

invalidateCode :: Connection -> CodeRecord -> IO ()
invalidateCode conn CodeRecord{codeValue} = do
  rowCount <- run conn query args
  assert (rowCount == 1) return ()
  where
    query = "update codes set valid = 0 where code = ?"
    args = [toSql codeValue]

convertCodeToToken :: Database -> CodeRecord -> IO TokenRecord
convertCodeToToken db code = withTransaction (dbConn db) $ \conn -> do
  invalidateCode conn code
  token <- secureRandom db
  insertToken conn token code

createToken :: Database -> Code -> IO (Maybe TokenRecord)
createToken db code = do
  maybeCode <- findCode (dbConn db) code
  case maybeCode of
    Nothing -> return Nothing
    Just record -> do
      now <- getCurrentTime
      if isCodeValidAt record now then
        Just <$> convertCodeToToken db record
      else return Nothing
