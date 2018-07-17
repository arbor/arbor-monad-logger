{-# LANGUAGE OverloadedStrings #-}

module Arbor.Monad.Logger
  ( logDebug
  , logInfo
  , logWarn
  , logError
  , logDebug'
  , logInfo'
  , logWarn'
  , logError'
  , MonadLogger(..)
  , LogLevel(..)
  ) where

import Control.Monad.Logger (LogLevel (..), MonadLogger, ToLogStr)

import qualified Control.Monad.Logger as L

logDebug :: MonadLogger m => String -> m ()
logDebug = logDebug'
{-# INLINE logDebug #-}

logInfo :: MonadLogger m => String -> m ()
logInfo = logInfo'
{-# INLINE logInfo #-}

logWarn :: MonadLogger m => String -> m ()
logWarn = logWarn'
{-# INLINE logWarn #-}

logError :: MonadLogger m => String -> m ()
logError = logError'
{-# INLINE logError #-}

logDebug' :: (MonadLogger m, ToLogStr s) => s -> m ()
logDebug' = L.logWithoutLoc "" LevelDebug
{-# INLINE logDebug' #-}

logInfo' :: (MonadLogger m, ToLogStr s) => s -> m ()
logInfo' = L.logWithoutLoc "" LevelInfo
{-# INLINE logInfo' #-}

logWarn' :: (MonadLogger m, ToLogStr s) => s -> m ()
logWarn' = L.logWithoutLoc "" LevelWarn
{-# INLINE logWarn' #-}

logError' :: (MonadLogger m, ToLogStr s) => s -> m ()
logError' = L.logWithoutLoc "" LevelError
{-# INLINE logError' #-}
