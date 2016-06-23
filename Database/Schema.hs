{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module Database.Schema where

import ClassyPrelude
import Database.Persist.TH

type EmailAddress = Text
type Token = Text

share [mkPersist sqlSettings] [persistLowerCase|
UserRow sql=users
  name Text
  email EmailAddress
  UniqueName name
  UniqueEmail email
  deriving Show
CodeRow sql=codes
  code Text
  generatedAt UTCTime sql="generated_at"
  valid Bool
  userId UserRowId sql="id_user"
  Primary code
  deriving Show
TokenRow sql=tokens
  token Token
  userId UserRowId sql="id_user"
  UniqueToken token
  deriving Show
PostRow sql=posts
  content Text
  at UTCTime
  userId UserRowId sql="id_user"
  parentId PostRowId Maybe sql="id_parent"
  deriving Show
|]
