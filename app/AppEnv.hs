{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module AppEnv where

import GHC.Generics

import qualified Arbor.Monad.Logger as L

data Logger = Logger
  { logger   :: L.TimedFastLogger
  , logLevel :: L.LogLevel
  } deriving (Generic)

newtype AppEnv = AppEnv
  { logger :: Logger
  } deriving (Generic)
