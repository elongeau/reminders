{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Server where

-- import           Prelude.Compat
-- import           Control.Monad.Except
-- import           Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Data.Aeson

-- import qualified Data.Aeson.Parser
-- import           Data.Aeson.Types
-- import           Data.Attoparsec.ByteString
-- import           Data.ByteString               (ByteString)
-- import           Data.List
-- import           Data.Maybe
-- import           Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics

import App
import Colog (LogAction, Message, Msg (..), Severity (..), filterBySeverity, richMessageAction)
import Control.Monad.Reader (MonadIO, runReaderT)
import Data.Pool
import Data.UUID
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import DB
import Env
import Log
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment (getEnv)

-- TODO split this module
data Remind = Remind
  { id           :: UUID
  , description  :: String
  , creationDate :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON Remind

type ReminderAPI = "reminds" :> Get '[JSON] [Remind]

getReminds
  :: (WithPool env m, WithLog env m)
  => m [Remind]
getReminds = selectM "SELECT * from reminders"

server :: ServerT ReminderAPI App
server = getReminds

reminderApi :: Proxy ReminderAPI
reminderApi = Proxy

nt :: AppEnv -> App a -> Handler a
nt env app = liftIO $ runReaderT (unApp app) env

app :: AppEnv -> Application
app env = serve reminderApi $ hoistServer reminderApi (nt env) server

instance FromRow Remind where
  fromRow = Remind <$> field <*> field <*> field

mkDbPool :: ConnectInfo -> IO DBPool
mkDbPool connectionInfo
         -- TODO use env variable
 = do
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

-- TODO add a pre-commit hook for stylish-haskell
main :: IO ()
main = do
  connectionInfo <- readConnectionInfo
  pool <- mkDbPool connectionInfo
  let env =
        Env
        { envDBPool = pool
        , envLogAction = mainLogAction Debug
        }
  run 8080 (app env)
