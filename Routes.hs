module Routes (Request(..), Response(..)) where

import           BasePrelude
import           Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import           Types

type Name = Text

data Request = GetPost ID
             | ListPosts (Maybe ID)
             | CreatePost (Maybe ID) Text
             | CreateCode EmailAddress
             | CreateToken Code
             | CreateUser Name EmailAddress

data Response = NewPost ResolvedPost
              | ExistingPost ResolvedPost
              | NewCode Code
              | NewToken Token
              | PostList [ResolvedPost]
              | NotLoggedIn
              | PostNotFound ID
              | InvalidUsername
              | BadRequest Lazy.Text
