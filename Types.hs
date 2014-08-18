module Types (
  Post(..),
  User, ID
) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import BasePrelude

data Post = Post { postID :: ID
                 , postBy :: User
                 , postContent :: Text
                 , postAt :: UTCTime
                 , postParentID :: Maybe ID
                 , postCount :: Int
                 }

instance Aeson.ToJSON Post where
  toJSON Post {..} = Aeson.object [ "id" .= postID
                                  , "content" .= postContent
                                  , "by" .= postBy
                                  , "at" .= postAt
                                  , "count" .= postCount
                                  , "idParent" .= postParentID
                                  ]

type User = Text
type ID = Int
