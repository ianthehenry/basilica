module Types (
  Thread(..),
  User, ID
) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import BasePrelude

data Thread = Thread { threadID :: ID
                     , threadBy :: User
                     , threadContent :: Text
                     , threadAt :: UTCTime
                     , threadParentID :: ID
                     , threadCount :: Int
                     }

instance Aeson.ToJSON Thread where
  toJSON Thread {..} = Aeson.object [ "id" .= threadID
                                    , "content" .= threadContent
                                    , "by" .= threadBy
                                    , "at" .= threadAt
                                    , "count" .= threadCount
                                    , "idParent" .= threadParentID
                                    ]

type User = Text
type ID = Int
