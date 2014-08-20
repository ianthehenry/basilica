module Database.Internal (
  module X,
  Connection,
  Database,
  insertRow
) where

import BasePrelude
import Control.Concurrent.Chan (Chan)
import Database.HDBC as X (SqlError(..), run, runRaw, withTransaction, quickQuery')
import Database.HDBC.SqlValue as X (SqlValue, fromSql, toSql)
import Database.HDBC.Sqlite3 (Connection)
import Types as X

type Database = (Connection, Chan Post)

insertRow :: Connection -> String -> [SqlValue] -> IO (Maybe ID)
insertRow rawConn query args = withTransaction rawConn $ \conn -> do
  inserted <- tryInsert conn
  if inserted then
    (Just . fromSql . head . head) <$> quickQuery' conn "select last_insert_rowid()" []
  else
    return Nothing
  where
    tryInsert conn = catchJust isForeignKeyError
      (run conn query args >> return True)
      (const $ return False)
    isForeignKeyError SqlError{seNativeError = 19} = Just ()
    isForeignKeyError _ = Nothing
