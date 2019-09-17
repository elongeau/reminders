{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Env where

import Colog (HasLog (..), LogAction, Message)
import Data.Pool
import Control.Monad.Reader (MonadReader, asks)
import Database.PostgreSQL.Simple (Connection)

type DBPool = Pool Connection

data Env m = Env
  { envDBPool    :: !DBPool
  , envLogAction :: !(LogAction m Message)
  }

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  
  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction logAction env =
    env
    { envLogAction = logAction
    }


class Has field env where
  obtain :: env -> field

instance Has DBPool (Env m) where
  obtain = envDBPool

instance Has (LogAction m Message) (Env m) where
  obtain = envLogAction

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
