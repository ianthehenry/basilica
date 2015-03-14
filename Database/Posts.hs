module Database.Posts (
  createPost,
  getPostsSince,
  getPost,
  postChildren,
) where

import BasePrelude
import Control.Concurrent.Chan
import Control.Monad.Reader (liftIO, ask)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Database.Internal

toPost :: [SqlValue] -> ResolvedPost
toPost [idPost, idUser, content, idParent, at, count, _, name, email] =
  ( Post { postID = fromSql idPost
         , postContent = fromSql content
         , postAt = fromSql at
         , postUserID = fromSql idUser
         , postParentID = fromSql idParent
         , postCount = fromSql count
         }
  , User { userID = fromSql idUser
         , userName = fromSql name
         , userEmail = fromSql email
         }
  )

postQuery :: Connection -> String -> [SqlValue] -> IO [ResolvedPost]
postQuery conn whereClause args = fmap toPost <$> quickQuery' conn query args
  where query = unlines [ "select posts.*, count(children.id), users.* from posts"
                        , "left outer join posts as children"
                        , "  on children.id_parent = posts.id"
                        , "inner join users on users.id = posts.id_user"
                        , whereClause
                        , "group by posts.id"
                        , "order by posts.id desc"
                        ]

getPost :: Database -> ID -> IO (Maybe ResolvedPost)
getPost Database{dbConn} idPost = listToMaybe <$>
  postQuery dbConn "where posts.id = ?" [toSql idPost]

postChildren :: Database -> ID -> IO [ResolvedPost]
postChildren Database{dbConn} idPost =
  postQuery dbConn "where posts.id_parent = ?" [toSql idPost]

getPostsSince :: Database -> Maybe ID -> IO [ResolvedPost]
getPostsSince Database{dbConn} Nothing = postQuery dbConn "" []
getPostsSince Database{dbConn} (Just idPost) =
  postQuery dbConn "where posts.id > ?" [toSql idPost]

insertPost :: User -> Text -> Maybe ID -> UTCTime -> DatabaseM (Maybe ResolvedPost)
insertPost User{userID = idUser} content idParent at = do
  db <- ask
  liftIO $ insertRow (dbConn db) query args >>= maybe (return Nothing) (report db)
  where
    report db idPost = do
      post <- fromJust <$> getPost db idPost
      writeChan (dbPostChan db) post
      return (Just post)
    query = unlines [ "insert into posts"
                    , "(id_user, content, id_parent, at)"
                    , "values (?, ?, ?, ?)"
                    ]
    args = [toSql idUser, toSql content, toSql idParent, toSql at]

createPost :: User -> Text -> Maybe ID -> DatabaseM (Maybe ResolvedPost)
createPost user content parentID =
  insertPost user content parentID =<< (liftIO getCurrentTime)
