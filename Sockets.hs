module Sockets (
  newServer,
  Path,
  Broadcaster
) where

import           BasePrelude
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Lazy (ByteString)
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.UUID (UUID)
import           Data.UUID.V4 (nextRandom)
import qualified Network.HTTP.Types.URI as URI
import qualified Network.WebSockets as WS
  
type Path = String
type Broadcaster = Path -> ByteString -> IO ()
data Client = Client { clientPath :: Path
                     , clientIdentifier :: UUID
                     , clientConnection :: WS.Connection
                     }
type ServerState = Map Path (Set Client)

instance Eq Client where
  (==) = (==) `on` clientIdentifier

instance Ord Client where
  (<=) = (<=) `on` clientIdentifier

newServerState :: ServerState
newServerState = Map.fromList $ (, Set.empty) <$> ["one", "two"]

addClient :: Client -> ServerState -> ServerState
addClient client@(Client {clientPath}) = Map.adjust (Set.insert client) clientPath

removeClient :: Client -> ServerState -> ServerState
removeClient client@(Client {clientPath}) = Map.adjust (Set.delete client) clientPath

broadcast :: Path -> ByteString -> ServerState -> IO ()
broadcast path message state =
  forM_ (state ! path) (flip WS.sendTextData message . clientConnection)

newServer :: IO (Broadcaster, WS.ServerApp)
newServer = do
  state <- newMVar newServerState
  return (makeBroadcast state, application state)
  where
    makeBroadcast db path msg =
      readMVar db >>= broadcast path msg

send :: Client -> Text -> IO ()
send = WS.sendTextData . clientConnection

ifAccept :: WS.PendingConnection -> (Client -> IO ()) -> IO ()
ifAccept pending callback =
  case (URI.decodePath . WS.requestPath . WS.pendingRequest) pending of
    ([], _) -> do
      -- todo: use Applicative?
      conn <- WS.acceptRequest pending
      uuid <- nextRandom
      callback Client { clientPath = "one"
                      , clientConnection = conn
                      , clientIdentifier = uuid
                      }
    _ -> WS.rejectRequest pending "You can only connect to / right now."

application :: MVar ServerState -> WS.ServerApp
application state pending = 
  ifAccept pending $ \client -> do
    send client "hello, friend"
    flip finally (disconnect client >> putStrLn "disconnected") $ do
      modifyMVar_ state (return . addClient client)
      readMVar state >>= broadcast "one" "new client!"
      talk state client
    where
      disconnect client = modifyMVar_ state (return . removeClient client)

talk :: MVar ServerState -> Client -> IO ()
talk state (Client { clientPath = path
                   , clientConnection = conn
                   }) = forever $ do
    msg <- WS.receiveData conn
    liftIO $ readMVar state >>= broadcast path ("message: " <> msg)
