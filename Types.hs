module Types (
  Post(..),
  CodeRecord(..),
  TokenRecord(..),
  User(..),
  ID, Email,
  Token, Code
) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import BasePrelude

data Post = Post { postID :: ID
                 , postBy :: Text
                 , postContent :: Text
                 , postAt :: UTCTime
                 , postParentID :: Maybe ID
                 , postCount :: Int
                 }

data CodeRecord = CodeRecord { codeValue :: Code
                             , codeGeneratedAt :: UTCTime
                             , codeValid :: Bool
                             , codeUserID :: ID
                             }

data TokenRecord = TokenRecord { tokenID :: ID
                               , tokenValue :: Token
                               , tokenUserID :: ID
                               }

data User = User { userID :: ID
                 , userEmail :: Text
                 }

instance Aeson.ToJSON Post where
  toJSON Post {..} = Aeson.object [ "id" .= postID
                                  , "content" .= postContent
                                  , "by" .= postBy
                                  , "at" .= postAt
                                  , "count" .= postCount
                                  , "idParent" .= postParentID
                                  ]

type ID = Int
type Email = Text
type Token = Text
type Code = Text
