module Types (
  Thread(..),
  User, ID, Path
) where

import BasePrelude
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))

data Thread = Thread { threadID :: ID
                     , threadBy :: User
                     , threadContent :: Text
                     , threadParentID :: ID
                     , threadAt :: UTCTime
                     }

instance Aeson.ToJSON Thread where
  toJSON Thread {..} = Aeson.object [ "id" .= threadID
                                    , "content" .= threadContent
                                    , "by" .= threadBy
                                    , "at" .= threadAt
                                    ]

type User = Text
type ID = Int
type Path = [ID]
