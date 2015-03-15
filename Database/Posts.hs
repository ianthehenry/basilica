module Database.Posts (
  createPost,
  getPostsSince,
  getPost,
) where

import BasePrelude
import Control.Monad.Reader (liftIO)
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

postQuery :: Int -> String -> [SqlValue] -> DatabaseM [ResolvedPost]
postQuery limit whereClause args = fmap toPost <$> runQuery query (args ++ [toSql limit])
  where query = unlines [ "select posts.*, count(children.id), users.* from posts"
                        , "left outer join posts as children"
                        , "  on children.id_parent = posts.id"
                        , "inner join users on users.id = posts.id_user"
                        , whereClause
                        , "group by posts.id"
                        , "order by posts.id desc"
                        , "limit ?"
                        ]

getPost :: ID -> DatabaseM (Maybe ResolvedPost)
getPost idPost = listToMaybe <$>
  postQuery 1 "where posts.id = ?" [toSql idPost]

getPostsSince :: Maybe ID -> Int -> DatabaseM [ResolvedPost]
getPostsSince Nothing limit = postQuery limit "" []
getPostsSince (Just idPost) limit =
  postQuery limit "where posts.id > ?" [toSql idPost]

insertPost :: User -> Text -> Maybe ID -> UTCTime -> DatabaseM (Maybe ResolvedPost)
insertPost User{userID = idUser} content idParent at =
  insertRow query args >>= maybe (return Nothing) getPost
  where
    query = unlines [ "insert into posts"
                    , "(id_user, content, id_parent, at)"
                    , "values (?, ?, ?, ?)"
                    ]
    args = [toSql idUser, toSql content, toSql idParent, toSql at]

createPost :: User -> Text -> Maybe ID -> DatabaseM (Maybe ResolvedPost)
createPost user content parentID =
  insertPost user content parentID =<< liftIO getCurrentTime
