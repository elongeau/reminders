{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain where

import Data.Aeson (ToJSON)
import Data.Time.Calendar (Day)
import Data.UUID (UUID)
import DB (FromRow (..), WithPool, field, fromRow, selectM)
import GHC.Generics (Generic)
import Log (WithLog)

data Remind = Remind
  { id           :: UUID
  , description  :: String
  , creationDate :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON Remind

instance FromRow Remind where
  fromRow = Remind <$> field <*> field <*> field

getReminds
  :: (WithPool env m, WithLog env m)
  => m [Remind]
getReminds = selectM "SELECT * from reminders"
