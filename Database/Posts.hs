module Database.Posts (
  createPost,
  getPostsSince,
  getPost,
  postChildren,
) where

import BasePrelude
import Control.Concurrent.Chan
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Database.Internal

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
                        , "  on children.id_parent = posts.id"
                        , whereClause
                        , "group by posts.id"
                        , "order by posts.id desc"
                        , ";"
                        ]

getPost :: Database -> ID -> IO (Maybe Post)
getPost (conn, _) idPost = listToMaybe <$>
  postQuery conn "where posts.id = ?" [toSql idPost]

postChildren :: Database -> ID -> IO [Post]
postChildren (conn, _) idPost = postQuery conn "where posts.id_parent = ?" [toSql idPost]

getPostsSince :: Database -> Maybe ID -> IO [Post]
getPostsSince (conn, _) Nothing = postQuery conn "" []
getPostsSince (conn, _) (Just idPost) = postQuery conn "where posts.id > ?" [toSql idPost]

insertPost :: Database -> Text -> Text -> Maybe ID -> UTCTime -> IO (Maybe Post)
insertPost db@(conn, newPosts) by content idParent at =
  insertRow conn query args >>= maybe (return Nothing) report
  where
    report idPost = do
      post <- fromJust <$> getPost db idPost
      writeChan newPosts post
      return (Just post)
    query = unlines [ "insert into posts"
                    , "(by, content, id_parent, at)"
                    , "values (?, ?, ?, ?)"
                    ]
    args = [toSql by, toSql content, toSql idParent, toSql at]

createPost :: Database -> Text -> Text -> Maybe ID -> IO (Maybe Post)
createPost db by content parentID =
  insertPost db by content parentID =<< getCurrentTime
