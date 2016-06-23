module Database.Internal
( module X
, Database(..)
, DatabaseM
, secureRandom
, asInt
, getID
, fromInt
, runInsert
, runQuery
, getOne
) where

import ClassyPrelude
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Control
import Crypto.Random.DRBG (genBytes, HashDRBG)
import Data.ByteString.Base16 as BS (encode)
import Database.Persist
import Database.Persist.Sql
import Database.Sqlite (SqliteException(..), Error(..))
import Types as X

data Database = Database { dbPool :: ConnectionPool
                         , dbRNG :: MVar HashDRBG
                         }

type DatabaseM a = ReaderT Database IO a

secureRandom :: DatabaseM Text
secureRandom = do
  rng <- asks dbRNG
  bytes <- liftIO $ modifyMVar rng $ \gen ->
    let Right (randomBytes, newGen) = genBytes 16 gen in
      pure (newGen, randomBytes)
  pure $ (decodeUtf8 . BS.encode) bytes

asInt :: PersistEntity a => Key a -> Int
asInt key = let [PersistInt64 x] = keyToValues key in fromIntegral x

getID :: PersistEntity a => Entity a -> Int
getID = asInt . entityKey

fromInt :: PersistEntity a => Int -> Key a
fromInt key = let Right x = keyFromValues [PersistInt64 (fromIntegral key)] in x

runInsert :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => a -> DatabaseM (Maybe (Key a))
runInsert entity = (Just <$> runQuery (insert entity)) `catch` swallowConstraintError
  where swallowConstraintError SqliteException{seError = ErrorConstraint} = pure Nothing
        swallowConstraintError e = throwM e

runQuery :: (MonadIO m, MonadReader Database m, MonadBaseControl IO m) => SqlPersistT m a -> m a
runQuery query = runSqlPool query =<< asks dbPool

getOne :: (MonadIO m, MonadReader Database m, MonadBaseControl IO m) => (a -> b) -> SqlPersistT m [a] -> m (Maybe b)
getOne f query = fmap f . listToMaybe <$> runQuery query
