{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE PatternSynonyms #-}

-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Server where

-- import           Prelude.Compat
-- import           Control.Monad.Except
-- import           Control.Monad.Reader
import           Data.Aeson
import Data.Text
-- import Control.Monad.IO.Class (liftIO)
-- import qualified Data.Aeson.Parser
-- import           Data.Aeson.Types
-- import           Data.Attoparsec.ByteString
-- import           Data.ByteString               (ByteString)
-- import           Data.List
-- import           Data.Maybe
-- import           Data.String.Conversions
import           Data.Time.Calendar
import           GHC.Generics

-- import           Lucid
-- import           Network.HTTP.Media            ((//), (/:))
import           Control.Monad.Reader (MonadReader, ReaderT, MonadIO, runReaderT)
import           Data.Pool
import           Data.UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Environment             ( getEnv )
import Colog (HasLog (..), LogAction, Message, Msg (..), Severity (..), filterBySeverity, richMessageAction, pattern D, log)
import GHC.Stack
import Env

-- TODO split this module

data Remind = Remind { id :: UUID
  , description           :: String
  , creationDate          :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON Remind

type ReminderAPI = "reminds" :> Get '[JSON] [Remind]


type AppEnv = Env App

newtype App a = App
  { unApp :: ReaderT AppEnv IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

type WithPool env m = (Has DBPool env, MonadReader env m, MonadIO m)

type WithLog env m = (MonadReader env m, HasLog env Message m, HasCallStack)

withPool :: (WithPool env m, WithLog env m) => (Connection -> IO a) -> m a
withPool f = do
  pool <- grab @DBPool
  liftIO $ withResource pool f

selectM :: (WithPool env m, FromRow a, WithLog env m) => Query -> m [a]
selectM queryStr = do
  Colog.log D $ "Run " <> pack (show queryStr)
  withPool $ \conn -> query_ conn queryStr

getReminds :: (WithPool env m, WithLog env m) => m [Remind]
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
mkDbPool connectionInfo = do
  -- TODO use env variable
  let create = connect connectionInfo
  createPool create close 1 0.5 1

readConnectionInfo :: IO ConnectInfo
readConnectionInfo =
  ConnectInfo
    <$> getEnv "DB_HOST"
    <*> (read <$> getEnv "DB_PORT")
    <*> getEnv "DB_USER"
    <*> getEnv "DB_PASSWORD"
    <*> getEnv "DB_DATABASE"

mainLogAction :: MonadIO m => Severity -> LogAction m Message
mainLogAction severity = filterBySeverity severity msgSeverity richMessageAction

-- TODO add a pre-commit hook for stylish-haskell
main :: IO ()
main = do
  connectionInfo <- readConnectionInfo
  pool           <- mkDbPool connectionInfo
  let env = Env {
     envDBPool = pool
      , envLogAction = mainLogAction Debug
    }
  run 8080 (app env)
