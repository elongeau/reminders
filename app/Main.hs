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
module Main where

import Env
import Server

import Colog (LogAction, Message, Msg (..), Severity (..), filterBySeverity, richMessageAction)
import Control.Monad.Reader (MonadIO)
import Data.Pool (createPool)
import Database.PostgreSQL.Simple (ConnectInfo (..), close, connect)
import Network.Wai.Handler.Warp (run)
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

mainLogAction
  :: MonadIO m
  => Severity -> LogAction m Message
mainLogAction severity = filterBySeverity severity msgSeverity richMessageAction

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
