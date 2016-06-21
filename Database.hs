{-# LANGUAGE FlexibleContexts #-}

module Database
( module X
, withDatabase
, DatabaseM
, Database(..)
) where

import ClassyPrelude
import Control.Monad.Logger (MonadLogger,)
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.Random.DRBG
import Database.Internal
import Database.Persist.Sql (withSqlPool, SqlBackend)
import Database.Persist.Sqlite (LogFunc, wrapConnection)
import Database.Posts as X
import Database.Sqlite (Connection, open, prepare, step)
import Database.Users as X
import Types as X

enableForeignKeys :: Connection -> IO ()
enableForeignKeys conn = prepare conn "PRAGMA foreign_keys = ON;" >>= void . step

createSqliteBackend :: Text -> LogFunc -> IO SqlBackend
createSqliteBackend connStr logFunc = do
  conn <- open connStr
  enableForeignKeys conn
  wrapConnection conn logFunc

withDatabase :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => Text -> (Database -> m a) -> m a
withDatabase dbPath f = do
  rng <- liftIO (newGenIO :: IO HashDRBG)
  rngSlot <- newMVar rng
  withSqlPool (createSqliteBackend dbPath) 10 $ \pool ->
    f Database { dbPool = pool
               , dbRNG = rngSlot
               }
