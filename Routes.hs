module Routes (Request(..), Response(..)) where

import           BasePrelude
import           Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import           Types

type Name = Text

data Request = GetPost ID
             | ListPosts (Maybe ID)
             | CreatePost (Maybe ID) Token Text
             | CreateCode EmailAddress
             | CreateToken Code
             | CreateUser Name EmailAddress

data Response = NewPost ResolvedPost
              | ExistingPost ResolvedPost
              | NewCode ResolvedCode
              | UnknownEmail
              | NewToken Token
              | PostList [ResolvedPost]
              | BadToken
              | PostNotFound ID
              | InvalidUsername
              | BadRequest Lazy.Text
