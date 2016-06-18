module Database.Internal (
  module X,
  Connection,
  Database(..),
  DatabaseM,
  secureRandom,
  runQuery,
  insertRow,
  insertRowRaw
) where

import           ClassyPrelude
import           Control.Monad.Reader (ReaderT, liftIO, ask)
import           Crypto.Random.DRBG (genBytes, HashDRBG)
import           Data.ByteString.Base16 as BS (encode)
import           Database.HDBC as X (SqlError(..), run, runRaw, withTransaction, quickQuery')
import           Database.HDBC.SqlValue as X (SqlValue, fromSql, toSql)
import           Database.HDBC.Sqlite3 (Connection)
import           Types as X

data Database = Database { dbConn :: Connection
                         , dbRNG :: MVar HashDRBG
                         }

type DatabaseM a = ReaderT Database IO a

runQuery :: String -> [SqlValue] -> DatabaseM [[SqlValue]]
runQuery query args = do
  Database{dbConn} <- ask
  liftIO (quickQuery' dbConn query args)

secureRandom :: Database -> IO Text
secureRandom Database{dbRNG} = do
  bytes <- modifyMVar dbRNG $ \gen ->
    let Right (randomBytes, newGen) = genBytes 16 gen in
      pure (newGen, randomBytes)
  pure $ (decodeUtf8 . BS.encode) bytes

insertRow :: String -> [SqlValue] -> DatabaseM (Maybe ID)
insertRow query args = do
  Database{dbConn} <- ask
  liftIO (insertRowRaw dbConn query args)

insertRowRaw :: Connection -> String -> [SqlValue] -> IO (Maybe ID)
insertRowRaw rawConn query args = withTransaction rawConn $ \conn -> do
  inserted <- tryInsert conn
  if inserted then do
    [[rowid]] <- quickQuery' conn "select last_insert_rowid()" []
    (pure . Just . fromSql) rowid
  else
    pure Nothing
  where
    tryInsert conn = catchJust isConstraintError
      (run conn query args $> True)
      (const $ pure False)
    isConstraintError SqlError{seNativeError = 19} = Just ()
    isConstraintError _ = Nothing
