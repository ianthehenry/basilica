module Main where

import BasePrelude hiding (app)
import Web.Scotty
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Data.Text (Text)
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
                       , messageThreadID :: ID
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
                                    , "creator" .= threadCreator
                                    , "timeStamp" .= threadTimeStamp
                                    ]

createThread :: MVar Forum -> IO Int -> Text -> Text -> IO Thread
createThread db nextID title creator = do
  timeStamp <- liftIO getCurrentTime
  threadID <- liftIO nextID
  let thread = Thread { threadID = threadID
                      , threadTimeStamp = timeStamp
                      , threadCreator = creator
                      , threadMessages = []
                      , threadTitle = title
                      }
  modifyMVar db (return . (, thread) . Map.insert threadID thread)

createMessage :: MVar Forum -> IO Int -> Text -> Text -> ID -> IO Message
createMessage db nextID text creator threadID = do
  timeStamp <- liftIO getCurrentTime
  messageID <- liftIO nextID
  let message = Message { messageID = messageID
                        , messageTimeStamp = timeStamp
                        , messageCreator = creator
                        , messageText = text
                        , messageThreadID = threadID
                        }
  modifyMVar db (return . (, message) . Map.update (Just . addMessage message) threadID)
  where
    addMessage message thread@(Thread {threadMessages}) =
      thread{threadMessages = message:threadMessages}

app :: MVar Forum -> IO Int -> IO Application
app database nextID = scottyApp $ do
  get "/threads/:id" $
    param "id" >>= withThread json
  get "/threads" $
    json =<< Map.elems <$> db
  post "/threads" $
    flip rescue (\msg -> status status400 >> text msg) $ do
      [title, creator] <- sequence $ param <$> ["title", "username"] 
      json =<< liftIO (createThread database nextID title creator)
  get "/threads/:id/messages" $
    param "id" >>= withThread (json . threadMessages)
  post "/threads/:id/messages" $ do
    threadID <- param "id"
    flip rescue (\msg -> status status400 >> text msg) $ do
      [title, creator] <- sequence $ param <$> ["title", "username"] 
      d <- db
      if Map.member threadID d then
        json =<< liftIO (createMessage database nextID title creator threadID)
      else thread404

  where
    db = liftIO (readMVar database)
    withThread f threadID =
      maybe thread404 f =<< Map.lookup threadID <$> db
    thread404 = status status404 >> text "Thread not found"
      
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
