{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import App
import Domain

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Network.Wai
import Servant ((:>), Get, Handler, JSON, Proxy (..), ServerT, hoistServer, serve)

type ReminderAPI = "reminds" :> Get '[JSON] [Remind]

server :: ServerT ReminderAPI App
server = getReminds

reminderApi :: Proxy ReminderAPI
reminderApi = Proxy

nt :: AppEnv -> App a -> Handler a
nt env app = liftIO $ runReaderT (unApp app) env

app :: AppEnv -> Application
app env = serve reminderApi $ hoistServer reminderApi (nt env) server
