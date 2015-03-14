module Sockets (
  newServer,
  Broadcaster
) where

import           BasePrelude hiding ((\\))
import           Control.Concurrent.Suspend (sDelay)
import           Control.Concurrent.Timer
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.Aeson as Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.UnixTime (UnixTime, getUnixTime, secondsToUnixDiffTime, diffUnixTime)
import qualified Network.HTTP.Types.URI as URI
import qualified Network.WebSockets as WS
import           System.IO.Streams.Attoparsec (ParseException)

import           Types

type Broadcaster = Post -> IO ()
data Client =
  Client { clientIdentifier :: Unique } deriving (Eq, Ord)
data Beat =
  Beat { beatLastTime :: UnixTime
       , beatConnection :: WS.Connection
       }
type ServerState = Map Client Beat

newServerState :: ServerState
newServerState = Map.empty

addClient :: Client -> Beat -> ServerState -> ServerState
addClient = Map.insert

removeClient :: Client -> ServerState -> ServerState
removeClient = Map.delete

broadcast :: Aeson.ToJSON a => a -> ServerState -> IO ()
broadcast message state =
  forM_ (Map.toList state) send
  where
    send (_, Beat { beatConnection } ) =
      WS.sendTextData beatConnection (Aeson.encode message)

ping :: WS.Connection -> IO ()
ping = flip WS.sendPing ("ping" :: ByteString)

heartbeatIntervalSeconds :: Int64
heartbeatIntervalSeconds = 20

heartbeat :: MVar ServerState -> IO ()
heartbeat db = modifyMVar_ db $ \state ->
  Map.fromList <$> filterM predicate (Map.toList state)
  where
    maximumDelta =
      secondsToUnixDiffTime (heartbeatIntervalSeconds * 2)
    predicate (_, Beat { beatLastTime, beatConnection } )= do
      now <- getUnixTime
      if diffUnixTime now beatLastTime > maximumDelta then do
        WS.sendClose beatConnection ("pong better" :: ByteString)
        return False
      else do
        ping beatConnection
        return True

newServer :: Aeson.ToJSON a => Chan a -> IO WS.ServerApp
newServer chan = do
  state <- newMVar newServerState
  repeatedTimer (heartbeat state) (sDelay heartbeatIntervalSeconds)
  forkIO $ getChanContents chan >>= mapM_ (makeBroadcast state)
  return $ application state
  where
    makeBroadcast db post = readMVar db >>= broadcast post

ifAccept :: WS.PendingConnection -> (WS.Connection -> IO ()) -> IO ()
ifAccept pending callback =
  case (URI.decodePath . WS.requestPath . WS.pendingRequest) pending of
    ([], _) -> WS.acceptRequest pending >>= callback
    _ -> WS.rejectRequest pending "You can only connect to / right now."

handleMessages :: IO () -> WS.Connection -> IO ()
handleMessages onPong conn = void $ (runMaybeT . forever) $ do
  msg <- lift $ WS.receive conn
  case msg of
    WS.DataMessage _     -> pure ()
    WS.ControlMessage cm -> case cm of
      WS.Close _ _ -> mzero
      WS.Pong _    -> lift onPong
      WS.Ping a    -> lift (WS.send conn (WS.ControlMessage (WS.Pong a)))

application_ :: MVar ServerState -> WS.ServerApp
application_ db pending = ifAccept pending $ \conn -> do
  clientIdentifier <- newUnique
  let client = Client { clientIdentifier }
  (`finally` disconnect client) $ do
    setTime client conn
    handleMessages (setTime client conn) conn
  where
    withState = modifyMVar_ db
    setTime client conn = withState $ \state -> do
      beatLastTime <- getUnixTime
      let beat = Beat { beatLastTime, beatConnection = conn }
      return $ addClient client beat state
    disconnect client = withState (return . removeClient client)

application :: MVar ServerState -> WS.ServerApp
application db pending =
  (handle connectionExceptions . handle parseExceptions) (application_ db pending)
  where
    parseExceptions =
      const $ throw WS.ConnectionClosed :: ParseException -> IO ()
    connectionExceptions =
      const $ return () :: WS.ConnectionException -> IO ()
