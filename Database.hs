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
import           Data.Time.Clock (getCurrentTime)
import           Types
import           Utils

type Database = (MVar (Map ID Thread), IO Int)

getThread :: Database -> ID -> IO (Maybe Thread)
getThread (db, _) idThread = Map.lookup idThread <$> readMVar db

listThreads :: Database -> IO [Thread]
listThreads (db, _) = fmap snd <$> Map.toList <$> readMVar db

createThread :: Database -> Text -> Text -> Maybe Thread -> IO Thread
createThread (db, nextID) content creator parent = do
  timeStamp <- liftIO getCurrentTime
  newID <- liftIO nextID
  let thread = Thread { threadID = newID
                      , threadAt = timeStamp
                      , threadBy = creator
                      , threadParentID = maybe 0 threadID parent
                      , threadContent = content
                      }
  modifyMVar db (return . (, thread) . Map.insert newID thread)

newDatabase :: IO Database
newDatabase = do
  db <- newMVar Map.empty
  idGen <- newMVar 0
  let generateID = modifyMVar idGen (return . dup . (+ 1))
  return (db, generateID)
