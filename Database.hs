{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database (
  module Types,
  Database,
  createThread,
  newDatabase,
  getThread,
  listThreads
) where

import           BasePrelude
import           Control.Concurrent.MVar
import           Control.Monad.Trans (liftIO)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (getCurrentTime)
import           Types
import           Utils

type Database = (MVar (Map ID Thread), IO Int)

getThread :: Database -> ID -> IO (Maybe Thread)
getThread (db, _) idThread = Map.lookup idThread <$> readMVar db

listThreads :: Database -> IO [Thread]
listThreads (db, _) = fmap snd <$> Map.toList <$> readMVar db

insertThread :: Database -> Thread -> IO Thread
insertThread (db, _) thread@(Thread {threadID}) =
  modifyMVar db (return . (, thread) . Map.insert threadID thread)

createThread :: Database -> Text -> Text -> Thread -> IO Thread
createThread db@(_, nextID) content creator (Thread {threadID = parentID}) = do
  timeStamp <- liftIO getCurrentTime
  newID <- ((parentID ++) . return . Text.pack . show) <$> liftIO nextID
  insertThread db Thread { threadID = newID
                         , threadAt = timeStamp
                         , threadBy = creator
                         , threadContent = content
                         }

newDatabase :: IO Database
newDatabase = do
  db <- newMVar Map.empty
  idGen <- newMVar 0
  let generateID = modifyMVar idGen (return . dup . (+ 1))
  let database = (db, generateID)

  timeStamp <- getCurrentTime
  let root = Thread { threadID = []
                    , threadAt = timeStamp
                    , threadBy = "god"
                    , threadContent = "Basilica"
                    }
  insertThread database root
  first <- createThread database "test post please ignore" "ian" root
  createThread database "well" "ian" first
  createThread database "isn't that something" "ian" first
  createThread database "a safe place for talking about haskell" "hao" root
  return database
