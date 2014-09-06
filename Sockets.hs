module Sockets (
  newServer,
  Broadcaster
) where

import           BasePrelude hiding ((\\))
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.Suspend (sDelay)
import           Control.Concurrent.Timer
import qualified Data.Aeson as Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Set (Set, (\\))
import qualified Data.Set as Set
import           Data.UUID (UUID)
import           Data.UUID.V4 (nextRandom)
import           Data.UnixTime (UnixTime, getUnixTime, secondsToUnixDiffTime, addUnixDiffTime)
import qualified Network.HTTP.Types.URI as URI
import qualified Network.WebSockets as WS
import           System.IO.Streams.Attoparsec (ParseException)

import           Types
import           Utils

type Broadcaster = Post -> IO ()
data Client = Client { clientIdentifier :: UUID
                     , clientLastPongTime :: UnixTime
                     , clientConnection :: WS.Connection
                     }
type ServerState = Set Client

instance Eq Client where
  (==) = (==) `on` clientIdentifier

instance Ord Client where
  (<=) = (<=) `on` clientIdentifier

newServerState :: ServerState
newServerState = Set.empty

addClient :: Client -> ServerState -> ServerState
addClient = Set.insert

removeClient :: Client -> ServerState -> ServerState
removeClient = Set.delete

broadcast :: Aeson.ToJSON a => a -> ServerState -> IO ()
broadcast message state =
  forM_ state (send (Aeson.encode message))

clientsOlderThan :: UnixTime -> Set Client -> Set Client
clientsOlderThan date = Set.filter ((< date) . clientLastPongTime)

modifyAndReturn :: MVar a -> (a -> a) -> IO a
modifyAndReturn mvar f = modifyMVar mvar (return . dup . f)

ping :: WS.Connection -> IO ()
ping = flip WS.sendPing ("ping" :: ByteString)

heartbeatIntervalSeconds :: Int64
heartbeatIntervalSeconds = 20

heartbeat :: MVar ServerState -> IO ()
heartbeat db = do
  state <- readMVar db
  now <- getUnixTime
  let maxPongTime = secondsToUnixDiffTime (-2 * heartbeatIntervalSeconds)
  let cutoff = addUnixDiffTime now maxPongTime
  let badClients = clientsOlderThan cutoff state
  forM_ badClients close
  goodClients <- modifyAndReturn db (\\ badClients)
  forM_ goodClients (ping . clientConnection)
  where
    close (Client {clientConnection}) =
      WS.sendClose clientConnection ("pong better" :: ByteString)

newServer :: Aeson.ToJSON a => Chan a -> IO WS.ServerApp
newServer chan = do
  state <- newMVar newServerState
  repeatedTimer (heartbeat state) (sDelay heartbeatIntervalSeconds)
  forkIO $ getChanContents chan >>= mapM_ (makeBroadcast state)
  return $ application state
  where
    makeBroadcast db post = readMVar db >>= broadcast post

send :: ByteString -> Client -> IO ()
send text client = WS.sendTextData (clientConnection client) text

ifAccept :: WS.PendingConnection -> (Client -> IO ()) -> IO ()
ifAccept pending callback =
  case (URI.decodePath . WS.requestPath . WS.pendingRequest) pending of
    ([], _) -> do
      -- todo: use Applicative?
      conn <- WS.acceptRequest pending
      uuid <- nextRandom
      time <- getUnixTime
      callback Client { clientConnection = conn
                      , clientIdentifier = uuid
                      , clientLastPongTime = time
                      }
    _ -> WS.rejectRequest pending "You can only connect to / right now."

handleMessages :: IO () -> WS.Connection -> IO ()
handleMessages onPong conn = WS.receive conn >>= \msg ->
  case msg of
    WS.DataMessage _     -> recurse
    WS.ControlMessage cm -> case cm of
      WS.Close _ -> return ()
      WS.Pong _  -> onPong >> recurse
      WS.Ping a  -> WS.send conn (WS.ControlMessage (WS.Pong a)) >> recurse
  where recurse = handleMessages onPong conn

application_ :: MVar ServerState -> WS.ServerApp
application_ db pending = ifAccept pending $ \client ->
  (`finally` (disconnect client)) $ do
    withState (return . addClient client)
    handleMessages (updatePong client) (clientConnection client)
  where
    withState = modifyMVar_ db
    setTime client state = do
      now <- getUnixTime
      -- because only the clientIdentifier matters,
      -- this replaces the previous client
      return $ Set.insert client{ clientLastPongTime = now } state
    disconnect client = withState (return . removeClient client)
    updatePong client = withState (setTime client)

application :: MVar ServerState -> WS.ServerApp
application db pending =
  (handle connectionExceptions . handle parseExceptions) (application_ db pending)
  where
    parseExceptions =
      const $ throw WS.ConnectionClosed :: ParseException -> IO ()
    connectionExceptions =
      const $ return () :: WS.ConnectionException -> IO ()
