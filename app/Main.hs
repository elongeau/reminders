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

-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Server where

-- import           Prelude.Compat
-- import           Control.Monad.Except
-- import           Control.Monad.Reader
import           Data.Aeson

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
import           Control.Monad.Reader
import           Data.Pool
import           Data.UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Environment             ( getEnv )
-- import           Servant.Types.SourceT          ( source )

-- TODO split this module

data Remind = Remind { id :: UUID
  , description           :: String
  , creationDate          :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON Remind

type ReminderAPI = "reminds" :> Get '[JSON] [Remind]

type DBPool = Pool Connection

newtype Env = Env {
    dbPool :: DBPool
    -- TODO add logging
  }

newtype App a = App
  { unApp :: ReaderT Env IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

class Has field env where
  obtain :: env -> field

instance Has DBPool Env where
  obtain = dbPool

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field

type WithPool env m = (Has DBPool env, MonadReader env m, MonadIO m)
withPool :: WithPool env m => (Connection -> IO a) -> m a
withPool f = do
  pool <- grab @DBPool
  liftIO $ withResource pool f

selectM :: (WithPool env m, FromRow a) => Query -> m [a]
selectM queryStr = withPool $ \conn -> query_ conn queryStr

getReminds :: WithPool env m => m [Remind]
getReminds = selectM "SELECT * from reminders"

server :: ServerT ReminderAPI App
server = getReminds

reminderApi :: Proxy ReminderAPI
reminderApi = Proxy

nt :: Env -> App a -> Handler a
nt env app = liftIO $ runReaderT (unApp app) env

app :: Env -> Application
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

-- TODO add a pre-commit hook for stylish-haskell
main :: IO ()
main = do
  connectionInfo <- readConnectionInfo
  pool           <- mkDbPool connectionInfo
  let env = Env { dbPool = pool }
  run 8080 (app env)
