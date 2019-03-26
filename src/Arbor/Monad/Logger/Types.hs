{-# LANGUAGE DeriveGeneric #-}
module Arbor.Monad.Logger.Types
( Logger(..)
, TimedFastLogger
, LogLevel(..)
)
where

import Control.Monad.Logger  (LogLevel)
import System.Log.FastLogger (TimedFastLogger)

import GHC.Generics (Generic)

data Logger = Logger
  { logger   :: TimedFastLogger
  , logLevel :: LogLevel
  } deriving (Generic)
