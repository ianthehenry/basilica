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

postQuery :: String -> [SqlValue] -> DatabaseM [ResolvedPost]
postQuery whereClause args = do
  Database{dbConn} <- ask
  fmap toPost <$> (liftIO $ quickQuery' dbConn query args)
  where query = unlines [ "select posts.*, count(children.id), users.* from posts"
                        , "left outer join posts as children"
                        , "  on children.id_parent = posts.id"
                        , "inner join users on users.id = posts.id_user"
                        , whereClause
                        , "group by posts.id"
                        , "order by posts.id desc"
                        ]

getPost :: ID -> DatabaseM (Maybe ResolvedPost)
getPost idPost = listToMaybe <$>
  postQuery "where posts.id = ?" [toSql idPost]

postChildren :: ID -> DatabaseM [ResolvedPost]
postChildren idPost =
  postQuery "where posts.id_parent = ?" [toSql idPost]

getPostsSince :: Maybe ID -> DatabaseM [ResolvedPost]
getPostsSince Nothing = postQuery "" []
getPostsSince (Just idPost) =
  postQuery "where posts.id > ?" [toSql idPost]

insertPost :: User -> Text -> Maybe ID -> UTCTime -> DatabaseM (Maybe ResolvedPost)
insertPost User{userID = idUser} content idParent at = do
  db <- ask
  (liftIO $ insertRow (dbConn db) query args) >>= maybe (return Nothing) (report db)
  where
    report db idPost = do
      post <- fromJust <$> getPost idPost
      liftIO $ writeChan (dbPostChan db) post
      return (Just post)
    query = unlines [ "insert into posts"
                    , "(id_user, content, id_parent, at)"
                    , "values (?, ?, ?, ?)"
                    ]
    args = [toSql idUser, toSql content, toSql idParent, toSql at]

createPost :: User -> Text -> Maybe ID -> DatabaseM (Maybe ResolvedPost)
createPost user content parentID =
  insertPost user content parentID =<< liftIO getCurrentTime
