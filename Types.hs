module Types (
  Post(..),
  CodeRecord(..),
  TokenRecord(..),
  User(..),
  ID, EmailAddress,
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
                 , userEmail :: EmailAddress
                 }

instance Aeson.ToJSON Post where
  toJSON Post {..} = Aeson.object [ "id" .= postID
                                  , "content" .= postContent
                                  , "by" .= postBy
                                  , "at" .= postAt
                                  , "count" .= postCount
                                  , "idParent" .= postParentID
                                  ]

instance Aeson.ToJSON TokenRecord where
  toJSON TokenRecord {..} = Aeson.object
   [ "id" .= tokenID
   , "token" .= tokenValue
   , "idUser" .= tokenUserID
   ]

type ID = Int
type EmailAddress = Text
type Token = Text
type Code = Text
