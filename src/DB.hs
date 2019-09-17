{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module DB (
  WithPool, selectM, Query, FromRow, fromRow, field
) where 

import Env
import Log

import Control.Monad.Reader (MonadIO, MonadReader, liftIO)
import Data.Text
import Data.Pool
import Database.PostgreSQL.Simple (Connection, Query, query_)
import           Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Colog (pattern D, log)

type WithPool env m = (Has DBPool env, MonadReader env m, MonadIO m)

withPool :: (WithPool env m, WithLog env m) => (Connection -> IO a) -> m a
withPool f = do
  pool <- grab @DBPool
  liftIO $ withResource pool f

selectM :: (WithPool env m, FromRow a, WithLog env m) => Query -> m [a]
selectM queryStr = do
  Colog.log D $ "Run " <> pack (show queryStr)
  withPool $ \conn -> query_ conn queryStr

