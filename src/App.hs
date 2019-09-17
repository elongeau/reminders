{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)

import Env

type AppEnv = Env App

newtype App a = App
  { unApp :: ReaderT AppEnv IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)
