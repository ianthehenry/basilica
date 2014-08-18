module Database (
  module Types,
  Database,
  createPost,
  newDatabase,
  getPost,
  postChildren,
  allPosts
) where

import BasePrelude
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Database.HDBC (SqlError(..), run, runRaw, withTransaction, quickQuery')
import Database.HDBC.SqlValue (SqlValue, fromSql, toSql)
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import Types

type Database = Connection

toPost :: [SqlValue] -> Post
toPost [idPost, by, content, idParent, at, count] =
  Post { postID = fromSql idPost
       , postContent = fromSql content
       , postAt = fromSql at
       , postBy = fromSql by
       , postParentID = fromSql idParent
       , postCount = fromSql count
       }

postQuery :: Connection -> String -> [SqlValue] -> IO [Post]
postQuery db whereClause args = fmap toPost <$> quickQuery' db query args
  where query = unlines [ "select posts.*, count(children.id) from posts"
                        , "left outer join posts as children"
                        , "  on children.parent_id = posts.id"
                        , whereClause
                        , "group by posts.id;"
                        ]

getPost :: Database -> ID -> IO (Maybe Post)
getPost db idPost = listToMaybe <$>
  postQuery db "where posts.id = ?" [toSql idPost]

postChildren :: Database -> ID -> IO [Post]
postChildren db idPost = postQuery db "where posts.parent_id = ?" [toSql idPost]

allPosts :: Database -> IO [Post]
allPosts db = postQuery db "" []

insertPost :: Database -> Text -> Text -> Maybe ID -> UTCTime -> IO (Maybe Post)
insertPost conn by content idParent at = withTransaction conn $ \db -> do
  inserted <- tryInsert db
  if inserted then do
    [lastRowID] <- head <$> quickQuery' db "select last_insert_rowid()" []
    getPost db (fromSql lastRowID)
  else
    return Nothing
  where
    tryInsert db = catchJust isForeignKeyError
      (run db query args >> return True)
      (\_ -> return False)
    isForeignKeyError SqlError{seNativeError = 19} = Just ()
    isForeignKeyError _ = Nothing
    query = unlines [ "insert into posts"
                    , "(by, content, parent_id, at)"
                    , "values (?, ?, ?, ?)"
                    ]
    args = [ toSql by
           , toSql content
           , toSql idParent
           , toSql at
           ]

createPost :: Database -> Text -> Text -> Maybe ID -> IO (Maybe Post)
createPost db by content parentID =
  insertPost db by content parentID =<< getCurrentTime

newDatabase :: IO Database
newDatabase = do
  conn <- connectSqlite3 "basilica.db"
  runRaw conn "COMMIT; PRAGMA foreign_keys = ON; BEGIN TRANSACTION;"
  return conn
