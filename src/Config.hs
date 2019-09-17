module Config where

import Env

import Colog (LogAction, Message, Msg (..), Severity (..), filterBySeverity, richMessageAction)
import Control.Monad.Reader (MonadIO)
import Data.Pool (createPool)
import Database.PostgreSQL.Simple (ConnectInfo (..), close, connect)
import System.Environment (getEnv)

data Config m = Config
  { cfgPool      :: !DBPool
  , cfgLogAction :: !(LogAction m Message)
  }

mkDbPool :: ConnectInfo -> IO DBPool
mkDbPool connectionInfo = do
  let create = connect connectionInfo
  createPool create close 1 0.5 1

readConnectionInfo :: IO ConnectInfo
readConnectionInfo =
  ConnectInfo <$> getEnv "DB_HOST" <*> (read <$> getEnv "DB_PORT") <*>
  getEnv "DB_USER" <*>
  getEnv "DB_PASSWORD" <*>
  getEnv "DB_DATABASE"

mainLogAction
  :: MonadIO m
  => Severity -> LogAction m Message
mainLogAction severity = filterBySeverity severity msgSeverity richMessageAction

mkConfig
  :: (MonadIO m)
  => IO (Config m)
mkConfig = do
  connectionInfo <- readConnectionInfo
  pool <- mkDbPool connectionInfo
  return
    Config
    { cfgPool = pool
    , cfgLogAction = mainLogAction Debug
    }
