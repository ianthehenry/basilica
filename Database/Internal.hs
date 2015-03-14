module Database.Internal (
  module X,
  Connection,
  Database(..),
  DatabaseM,
  secureRandom,
  insertRow
) where

import           BasePrelude
import           Control.Concurrent.Chan (Chan)
import           Control.Concurrent.MVar
import           Control.Monad.Reader (ReaderT, liftIO, ask)
import           Crypto.Random.DRBG (genBytes, HashDRBG)
import           Data.ByteString.Base16 as BS (encode)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Database.HDBC as X (SqlError(..), run, runRaw, withTransaction, quickQuery')
import           Database.HDBC.SqlValue as X (SqlValue, fromSql, toSql)
import           Database.HDBC.Sqlite3 (Connection)
import           Types as X

data Database = Database { dbConn :: Connection
                         , dbPostChan :: Chan ResolvedPost
                         , dbRNG :: MVar HashDRBG
                         }

type DatabaseM a = ReaderT Database IO a

secureRandom :: Database -> IO Text
secureRandom Database {dbRNG} = do
  bytes <- modifyMVar dbRNG $ \gen ->
    let Right (randomBytes, newGen) = genBytes 16 gen in
      return (newGen, randomBytes)
  return $ (Text.decodeUtf8 . BS.encode) bytes

insertRow :: Connection -> String -> [SqlValue] -> IO (Maybe ID)
insertRow rawConn query args = withTransaction rawConn $ \conn -> do
  inserted <- tryInsert conn
  if inserted then
    (Just . fromSql . head . head) <$> quickQuery' conn "select last_insert_rowid()" []
  else
    return Nothing
  where
    tryInsert conn = catchJust isConstraintError
      (run conn query args >> return True)
      (const $ return False)
    isConstraintError SqlError{seNativeError = 19} = Just ()
    isConstraintError _ = Nothing
