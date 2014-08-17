module Database (
  module Types,
  Database,
  createThread,
  newDatabase,
  getThread,
  threadChildren,
  allThreads
) where

import BasePrelude
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Database.HDBC (run, withTransaction, quickQuery')
import Database.HDBC.SqlValue (SqlValue, fromSql, toSql)
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import Types

type Database = Connection

toThread :: [SqlValue] -> Thread
toThread [idThread, by, content, idParent, at, count] =
  Thread { threadID = fromSql idThread
         , threadContent = fromSql content
         , threadAt = fromSql at
         , threadBy = fromSql by
         , threadParentID = fromSql idParent
         , threadCount = fromSql count
         }
  
threadQuery :: Connection -> String -> [SqlValue] -> IO [Thread]
threadQuery db whereClause args = fmap toThread <$> quickQuery' db query args
  where query = unlines [ "select threads.*, count(children.id) from threads"
                        , "left outer join threads as children"
                        , "  on children.parent_id = threads.id"
                        , whereClause
                        , "group by threads.id;"
                        ]

getThread :: Database -> ID -> IO (Maybe Thread)
getThread db idThread = listToMaybe <$>
  threadQuery db "where threads.id = ?" [toSql idThread]

threadChildren :: Database -> ID -> IO [Thread]
threadChildren db idThread = threadQuery db "where threads.parent_id = ?" [toSql idThread]

allThreads :: Database -> IO [Thread]
allThreads db = threadQuery db "" []

insertThread :: Database -> Text -> Text -> ID -> UTCTime -> IO Thread
insertThread conn by content idParent at = withTransaction conn $ \db -> do
  run db "insert into threads (by, content, parent_id, at) values (?, ?, ?, ?)" args
  [lastRowID] <- head <$> quickQuery' db "select last_insert_rowid()" []
  fromJust <$> getThread db (fromSql lastRowID)
  where args = [ toSql by
               , toSql content
               , toSql idParent
               , toSql at
               ]

createThread :: Database -> Text -> Text -> Thread -> IO Thread
createThread db by content (Thread {threadID = parentID}) =
  insertThread db by content parentID =<< getCurrentTime

newDatabase :: IO Database
newDatabase = connectSqlite3 "basilica.db"
