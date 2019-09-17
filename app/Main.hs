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

import Config
import Env
import Server

import Colog (LogAction, Message, Msg (..), Severity (..), filterBySeverity, richMessageAction)
import Control.Monad.Reader (MonadIO)
import Network.Wai.Handler.Warp (run)

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
