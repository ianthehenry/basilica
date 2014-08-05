{-# LANGUAGE RecordWildCards #-}

module Main where

import BasePrelude hiding (app)
import Web.Scotty
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Concurrent.MVar
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Control.Monad.Trans (liftIO)
import Network.HTTP.Types

type User = Text
type ID = Int

data Thread = Thread { threadID :: ID
                     , threadTitle :: Text
                     , threadMessages :: [Message]
                     , threadCreator :: User
                     , threadTimeStamp :: UTCTime
                     }
type Forum = Map ID Thread
data Message = Message { messageID :: ID
                       , messageCreator :: User
                       , messageText :: Text
                       , messageTimeStamp :: UTCTime
                       }

instance Aeson.ToJSON Message where
  toJSON Message {..} = Aeson.object [ "id" .= messageID
                                     , "creator" .= messageCreator
                                     , "text" .= messageText
                                     , "timeStamp" .= messageTimeStamp
                                     ]

instance Aeson.ToJSON Thread where
  toJSON Thread {..} = Aeson.object [ "id" .= threadID
                                    , "title" .= threadTitle
                                    , "messages" .= threadMessages
                                    , "creator" .= threadCreator
                                    , "timeStamp" .= threadTimeStamp
                                    ]

app :: MVar Forum -> IO Int -> IO Application
app database nextID = scottyApp $ do
  get "/threads/:id" $ do
    threadID <- param "id"
    entity "Thread not found" =<< Map.lookup threadID <$> db
  get "/threads" $
    json =<< Map.elems <$> db
  post "/threads" $
    flip rescue (\msg -> status status400 >> text msg) $ do
      title <- param "title"
      creator <- param "username"
      timeStamp <- liftIO getCurrentTime
      threadID <- liftIO nextID
      let thread = Thread { threadID = threadID
                          , threadTimeStamp = timeStamp
                          , threadCreator = creator
                          , threadMessages = []
                          , threadTitle = title
                          }
      liftIO $ modifyMVar_ database (return . Map.insert threadID thread)
      json thread

  where
    db = liftIO (readMVar database)
    entity msg = maybe (status status404 >> text msg) json

--postComment :: Thread -> Thread

twice :: a -> (a, a)
twice a = (a, a)

main :: IO ()
main = do
  let port = 3000
  database <- newMVar Map.empty
  idGen <- newMVar 0
  let generateID = modifyMVar idGen (return . twice . (+ 1))
  putStrLn $ "Running on port " ++ show port
  Warp.run port =<< app database generateID
