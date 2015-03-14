module Database (
  module X,
  newDatabase,
  DatabaseM,
  Database(..)
) where

import BasePrelude
import Crypto.Random.DRBG
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.Internal
import Database.Posts as X
import Database.Users as X
import Types as X

newDatabase :: FilePath -> IO Database
newDatabase path = do
  conn <- connectSqlite3 path
  runRaw conn "COMMIT; PRAGMA foreign_keys = ON; BEGIN TRANSACTION;"
  newPosts <- newChan
  rng <- newGenIO :: IO HashDRBG
  rngSlot <- newMVar rng
  return Database { dbConn = conn
                  , dbPostChan = newPosts
                  , dbRNG = rngSlot
                  }
