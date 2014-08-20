module Database (
  module X,
  newDatabase,
  Database
) where

import BasePrelude
import Control.Concurrent.Chan (newChan)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.Internal
import Database.Posts as X
import Database.Users as X
import System.FilePath
import Types as X

newDatabase :: FilePath -> IO Database
newDatabase path = do
  conn <- connectSqlite3 path
  runRaw conn "COMMIT; PRAGMA foreign_keys = ON; BEGIN TRANSACTION;"
  newPosts <- newChan
  return (conn, newPosts)
