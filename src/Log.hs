{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}

module Log where

import Colog (HasLog (..), Message)
import Control.Monad.Reader (MonadReader)
import GHC.Stack

type WithLog env m = (MonadReader env m, HasLog env Message m, HasCallStack)
