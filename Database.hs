module Database (
  module Types,
  Database,
  createPost,
  getPostsSince,
  newDatabase,
  getPost,
  postChildren,
) where

import BasePrelude
import Control.Concurrent.Chan
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Database.HDBC (SqlError(..), run, runRaw, withTransaction, quickQuery')
import Database.HDBC.SqlValue (SqlValue, fromSql, toSql)
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import System.FilePath
import Types

type Database = (Connection, Chan Post)

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
postQuery conn whereClause args = fmap toPost <$> quickQuery' conn query args
  where query = unlines [ "select posts.*, count(children.id) from posts"
                        , "left outer join posts as children"
                        , "  on children.parent_id = posts.id"
                        , whereClause
                        , "group by posts.id;"
                        ]

getPost :: Database -> ID -> IO (Maybe Post)
getPost (conn, _) idPost = listToMaybe <$>
  postQuery conn "where posts.id = ?" [toSql idPost]

postChildren :: Database -> ID -> IO [Post]
postChildren (conn, _) idPost = postQuery conn "where posts.parent_id = ?" [toSql idPost]

getPostsSince :: Database -> Maybe ID -> IO [Post]
getPostsSince (conn, _) Nothing = postQuery conn "" []
getPostsSince (conn, _) (Just idPost) = postQuery conn "where posts.id > ?" [toSql idPost]

insertPost :: Database -> Text -> Text -> Maybe ID -> UTCTime -> IO (Maybe Post)
insertPost db@(rawConn, newPosts) by content idParent at = withTransaction rawConn $ \conn -> do
  inserted <- tryInsert conn
  if inserted then do
    [lastRowID] <- head <$> quickQuery' conn "select last_insert_rowid()" []
    post <- fromJust <$> getPost db (fromSql lastRowID)
    writeChan newPosts post
    return (Just post)
  else
    return Nothing
  where
    tryInsert conn = catchJust isForeignKeyError
      (run conn query args >> return True)
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

newDatabase :: FilePath -> IO Database
newDatabase path = do
  conn <- connectSqlite3 path
  runRaw conn "COMMIT; PRAGMA foreign_keys = ON; BEGIN TRANSACTION;"
  newPosts <- newChan
  return (conn, newPosts)
