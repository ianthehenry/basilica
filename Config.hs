module Config where

import           BasePrelude
import           Data.ByteString (ByteString)
import qualified Data.Configurator as Conf
import qualified Data.Text as Strict

data Config = Config { confPort :: Int
                     , confDBPath :: String
                     , confClientUrl :: Strict.Text
                     , confClientOrigin :: Maybe ByteString
                     , confMandrillKey :: Maybe Strict.Text
                     }

loadConfig :: IO Config
loadConfig = do
  conf <- Conf.load [Conf.Required "conf"]

  port <- Conf.require conf "port"
  dbpath <- Conf.require conf "dbpath"
  clientUrl <- Conf.require conf "client-url"
  clientOrigin <- Conf.lookup conf "client-origin"
  mandrillKey <- Conf.lookup conf "mandrill-key"

  return Config { confPort = port
                , confDBPath = dbpath
                , confClientUrl = clientUrl
                , confClientOrigin = clientOrigin
                , confMandrillKey = mandrillKey
                }

