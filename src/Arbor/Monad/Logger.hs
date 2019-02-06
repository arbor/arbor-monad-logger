{-# LANGUAGE CPP               #-}
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
  , pushLogMessage
  , withStdOutTimedFastLogger
  , runTimedLogT
  , runTimedFastLoggerLoggingT
  , LogLevel(..)
  , LoggingT(..)
  , MonadLogger(..)
  , TimedFastLogger(..)
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger   hiding (logDebug, logError, logInfo, logWarn)
import System.Log.FastLogger

import qualified Control.Monad.Logger  as L
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T

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

pushLogMessage :: (ToLogStr s) => TimedFastLogger -> LogLevel -> s -> IO ()
pushLogMessage t l s = t (defaultTimedLogStr defaultLoc "" l (toLogStr s))

defaultTimedLogStr :: Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> FormattedTime
#if MIN_VERSION_fast_logger(0, 2, 0)
              -> LogStr
#else
              -> BS.ByteString
#endif
defaultTimedLogStr loc src level msg time =
#if MIN_VERSION_fast_logger(0, 2, 0)
    "[" `mappend` defaultLogLevelStr level `mappend`
    (if T.null src
        then mempty
        else "#" `mappend` toLogStr src) `mappend`
    "] " `mappend` "[" `mappend` toLogStr time `mappend` "] " `mappend`
    msg `mappend`
    (if isDefaultLoc loc
        then "\n"
        else
            " @(" `mappend`
            toLogStr (BS.pack (fileLocStr loc)) `mappend`
            ")\n")
#else
    BS.concat
        [ BS.pack "["
        , case level of
            LevelOther t -> encodeUtf8 t
            _            -> encodeUtf8 $ pack $ drop 5 $ show level
        , if T.null src
            then BS.empty
            else encodeUtf8 $ '#' `T.cons` src
        , BS.pack "] "
        , BS.pack "["
        , time
        , BS.pack "] "
        , case msg of
            LS s -> encodeUtf8 $ pack s
            LB b -> b
        , BS.pack " @("
        , encodeUtf8 $ pack (fileLocStr loc)
        , BS.pack ")\n"
        ]
#endif

-- taken from file-location package
-- turn the TH Loc loaction information into a human readable string
-- leaving out the loc_end parameter
fileLocStr :: Loc -> String
fileLocStr loc = loc_package loc ++ ':' : loc_module loc ++
  ' ' : loc_filename loc ++ ':' : line loc ++ ':' : char loc
  where line = show . fst . loc_start
        char = show . snd . loc_start

defaultLogLevelStr :: LogLevel -> LogStr
defaultLogLevelStr level = case level of
    LevelOther t -> toLogStr t
    _            -> toLogStr $ BS.pack $ drop 5 $ show level

isDefaultLoc :: Loc -> Bool
isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) = True
isDefaultLoc _                                                     = False

withStdOutTimedFastLogger :: (TimedFastLogger -> IO a) -> IO a
withStdOutTimedFastLogger f = do
  tc <- newTimeCache "%Y-%m-%d %T"
  withTimedFastLogger tc (LogStdout defaultBufSize) $ \logger -> f logger

runTimedLogT :: MonadIO m => LogLevel -> TimedFastLogger -> LoggingT m a -> m a
runTimedLogT logLevel logger =
  runTimedFastLoggerLoggingT logger . filterLogger (\_ lvl -> lvl >= logLevel)

-- | Run a block using a 'TimedFastLogger'.
runTimedFastLoggerLoggingT :: TimedFastLogger -> LoggingT m a -> m a
runTimedFastLoggerLoggingT tfl m = runLoggingT m $ \a b c d -> tfl (defaultTimedLogStr a b c d)
