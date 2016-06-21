module Database.Posts
( createPost
, getPosts
, getPost
) where

import ClassyPrelude hiding (groupBy, on)
import Database.Esqueleto
import Database.Internal
import Database.Schema

toResolvedPost :: (Entity PostRow, Value Int, Entity UserRow) -> ResolvedPost
toResolvedPost (postEntity, childCount, userEntity) = ResolvedPost post user
  where
    postRow = entityVal postEntity
    userRow = entityVal userEntity
    post = Post { postID = getID postEntity
                , postUserID = asInt (postRowUserId postRow)
                , postContent = postRowContent postRow
                , postAt = postRowAt postRow
                , postParentID = asInt <$> postRowParentId postRow
                , postCount = unValue childCount
                }
    user = User { userID = getID userEntity
                , userName = userRowName userRow
                , userEmail = userRowEmail userRow
                }

postQuery :: (SqlExpr (Entity PostRow) -> SqlQuery ()) -> DatabaseM [ResolvedPost]
postQuery customClause = fmap toResolvedPost <$> runQuery query
  where query = select $ from $ \((post `LeftOuterJoin` child) `InnerJoin` user) -> do
          on $ (user ^. UserRowId) ==. (post ^. PostRowUserId)
          on $ (child ^. PostRowParentId) ==. just (post ^. PostRowId)
          customClause post
          groupBy (post ^. PostRowId)
          orderBy [desc $ post ^. PostRowId]
          pure (post, count (child ^. PostRowId), user)

getPostByKey :: PostRowId -> DatabaseM (Maybe ResolvedPost)
getPostByKey idPost = listToMaybe <$> postQuery (\post -> where_ $ post ^. PostRowId ==. val idPost)

getPost :: Int -> DatabaseM (Maybe ResolvedPost)
getPost = getPostByKey . fromInt

getPosts :: PostQuery -> DatabaseM [ResolvedPost]
getPosts PostQuery{..} = postQuery $ \post -> do
  maybe (pure ()) (\a -> where_ (post ^. PostRowId >. val a)) (fromInt <$> postQueryAfter)
  maybe (pure ()) (\b -> where_ (post ^. PostRowId <. val b)) (fromInt <$> postQueryBefore)
  limit (fromIntegral postQueryLimit)

insertPost :: User -> Text -> Maybe ID -> UTCTime -> DatabaseM (Maybe ResolvedPost)
insertPost User{userID = idUser} content idParent at = do
  key <- runInsert postRow
  maybe (pure Nothing) getPostByKey key
  where postRow = PostRow { postRowUserId = fromInt idUser
                          , postRowContent = content
                          , postRowParentId = fromInt <$> idParent
                          , postRowAt = at
                          }

createPost :: User -> Text -> Maybe ID -> DatabaseM (Maybe ResolvedPost)
createPost user content parentID =
  insertPost user content parentID =<< liftIO getCurrentTime
