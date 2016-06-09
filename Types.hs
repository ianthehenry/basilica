{-# LANGUAGE FlexibleInstances #-}

module Types (
  Post(..),
  PostQuery(..),
  CodeRecord(..),
  TokenRecord(..),
  User(..),
  ResolvedPost(..),
  ResolvedCode(..),
  ResolvedToken(..),
  ID, EmailAddress,
  Token, Code
) where

import           BasePrelude
import qualified Crypto.Hash.MD5 as MD5
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base16 as Hex
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock (UTCTime)

data PostQuery = PostQuery { postQueryBefore :: Maybe ID
                           , postQueryAfter :: Maybe ID
                           , postQueryLimit :: Int
                           }

data Post = Post { postID :: ID
                 , postUserID :: ID
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
                 , userName :: Text
                 , userEmail :: EmailAddress
                 }

data ResolvedPost = ResolvedPost Post User
data ResolvedCode = ResolvedCode CodeRecord User
data ResolvedToken = ResolvedToken TokenRecord User

postPairs :: Post -> [Aeson.Pair]
postPairs Post{..} = [ "id" .= postID
                     , "content" .= postContent
                     , "at" .= postAt
                     , "count" .= postCount
                     , "idParent" .= postParentID
                     ]

instance Aeson.ToJSON Post where
  toJSON post@(Post{..}) = Aeson.object
    ("idUser" .= postUserID : postPairs post)

gravatar :: Text -> Text
gravatar = Text.decodeUtf8 . Hex.encode . MD5.hash . Text.encodeUtf8

instance Aeson.ToJSON User where
  toJSON User{..} = Aeson.object
    [ "id" .= userID
    , "name" .= userName
    , "face" .= Aeson.object ["gravatar" .= gravatar userEmail]
    ]

instance Aeson.ToJSON ResolvedPost where
  toJSON (ResolvedPost post@(Post{..}) user) = Aeson.object
    ("user" .= user : postPairs post)

instance Aeson.ToJSON ResolvedToken where
  toJSON (ResolvedToken TokenRecord{..} user) = Aeson.object
    [ "id" .= tokenID
    , "token" .= tokenValue
    , "user" .= user
    ]

type ID = Int
type EmailAddress = Text
type Token = Text
type Code = Text
