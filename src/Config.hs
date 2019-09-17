module Config where

import Env

import Data.Pool (createPool)
import Database.PostgreSQL.Simple (ConnectInfo (..), close, connect)
import System.Environment (getEnv)

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
