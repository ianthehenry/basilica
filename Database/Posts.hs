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
toPost [idPost, idUser, content, idParent, at, count] =
  Post { postID = fromSql idPost
       , postContent = fromSql content
       , postAt = fromSql at
       , postUserID = fromSql idUser
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
                        ]

getPost :: Database -> ID -> IO (Maybe Post)
getPost Database{dbConn} idPost = listToMaybe <$>
  postQuery dbConn "where posts.id = ?" [toSql idPost]

postChildren :: Database -> ID -> IO [Post]
postChildren Database{dbConn} idPost =
  postQuery dbConn "where posts.id_parent = ?" [toSql idPost]

getPostsSince :: Database -> Maybe ID -> IO [Post]
getPostsSince Database{dbConn} Nothing = postQuery dbConn "" []
getPostsSince Database{dbConn} (Just idPost) =
  postQuery dbConn "where posts.id > ?" [toSql idPost]

insertPost :: Database -> User -> Text -> Maybe ID -> UTCTime -> IO (Maybe Post)
insertPost db@(Database{dbConn, dbPostChan}) User{userID = idUser} content idParent at =
  insertRow dbConn query args >>= maybe (return Nothing) report
  where
    report idPost = do
      post <- fromJust <$> getPost db idPost
      writeChan dbPostChan post
      return (Just post)
    query = unlines [ "insert into posts"
                    , "(id_user, content, id_parent, at)"
                    , "values (?, ?, ?, ?)"
                    ]
    args = [toSql idUser, toSql content, toSql idParent, toSql at]

createPost :: Database -> User -> Text -> Maybe ID -> IO (Maybe Post)
createPost db user content parentID =
  insertPost db user content parentID =<< getCurrentTime
