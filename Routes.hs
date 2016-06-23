module Routes
( Request(..)
, Response(..)
) where

import ClassyPrelude
import Types

type Name = Text

data Request = GetPost ID
             | ListPosts PostQuery
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
              | BadRequest LText
              | PostNotFound ID
