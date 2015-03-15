module Routes (Request(..), Response(..)) where

import           BasePrelude
import           Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import           Types

type Name = Text

data Request = GetPost ID
             | ListPosts (Maybe ID) Int
             | CreatePost (Maybe ID) Token Text
             | CreateCode EmailAddress
             | CreateToken Code
             | CreateUser EmailAddress Name

data Response = NewPost ResolvedPost
              | ExistingPost ResolvedPost
              | PostList [ResolvedPost]
              | NewToken ResolvedToken
              | NewUser ResolvedCode
              | NewCode ResolvedCode
              | BadToken
              | BadCode
              | UnknownEmail
              | InvalidUsername
              | ExistingNameOrEmail
              | BadRequest Lazy.Text
              | PostNotFound ID
