{-# LANGUAGE FlexibleInstances #-}

module Types (
  Post(..),
  CodeRecord(..),
  TokenRecord(..),
  User(..),
  ID, EmailAddress,
  Token, Code
) where

import           BasePrelude
import qualified Crypto.Hash.MD5 as MD5
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Hex
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock (UTCTime)

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

gravatar :: Text -> Text
gravatar = Text.decodeUtf8 . Hex.encode . MD5.hash . Text.encodeUtf8

instance Aeson.ToJSON User where
  toJSON User{..} = Aeson.object
    [ "id" .= userID
    , "username" .= ("anon" :: Text)
    , "face" .= Aeson.object ["gravatar" .= gravatar userEmail]
    ]

instance Aeson.ToJSON (TokenRecord, User) where
  toJSON (TokenRecord{..}, user) = Aeson.object
    [ "id" .= tokenID
    , "token" .= tokenValue
    , "user" .= user
    , "idUser" .= tokenUserID
    ]

type ID = Int
type EmailAddress = Text
type Token = Text
type Code = Text
