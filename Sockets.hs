module Sockets (
  newServer,
  Broadcaster
) where

import           BasePrelude
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Data.UUID (UUID)
import           Data.UUID.V4 (nextRandom)
import qualified Network.HTTP.Types.URI as URI
import qualified Network.WebSockets as WS
import           Types

type Path = ID

type Broadcaster = Thread -> IO ()
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
newServerState = Map.fromList [([], Set.empty)]

addClient :: Client -> ServerState -> ServerState
addClient client@(Client {clientPath}) = Map.adjust (Set.insert client) clientPath

removeClient :: Client -> ServerState -> ServerState
removeClient client@(Client {clientPath}) = Map.adjust (Set.delete client) clientPath

broadcast :: Aeson.ToJSON a => a -> Path -> ServerState -> IO ()
broadcast message path state =
  forM_ (Map.findWithDefault Set.empty path state) (send (Aeson.encode message))

newServer :: IO (Broadcaster, WS.ServerApp)
newServer = do
  state <- newMVar newServerState
  return (makeBroadcast state, application state)
  where
    makeBroadcast db thread@(Thread {threadID}) = do
      database <- readMVar db
      forM_ (inits threadID) (flip (broadcast thread) database)

send :: ByteString -> Client -> IO ()
send text client = WS.sendTextData (clientConnection client) text

ifAccept :: WS.PendingConnection -> (Client -> IO ()) -> IO ()
ifAccept pending callback =
  case (URI.decodePath . WS.requestPath . WS.pendingRequest) pending of
    ([], _) -> do
      -- todo: use Applicative?
      conn <- WS.acceptRequest pending
      uuid <- nextRandom
      callback Client { clientPath = []
                      , clientConnection = conn
                      , clientIdentifier = uuid
                      }
    _ -> WS.rejectRequest pending "You can only connect to / right now."

alert :: Text -> Aeson.Value
alert msg = Aeson.object ["msg" .= msg]

application :: MVar ServerState -> WS.ServerApp
application state pending =
  ifAccept pending $ \client ->
    flip finally (disconnect client >> putStrLn "disconnected") $ do
      modifyMVar_ state (return . addClient client)
      readMVar state >>= broadcast (alert "new client") []
    where
      disconnect client = modifyMVar_ state (return . removeClient client)
