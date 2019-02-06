{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Arbor.Monad.Logger
import Control.Monad.Reader

import qualified AppEnv as E

newtype Service a = Service
  { unService :: ReaderT E.AppEnv (LoggingT IO) a
  }
  deriving  ( Functor
            , Applicative
            , Monad
            , MonadIO
            , MonadLogger
            , MonadReader E.AppEnv)

runServiceTimedFastLogger :: e -> LogLevel -> ReaderT e (LoggingT IO) a -> IO a
runServiceTimedFastLogger e lvl f = withStdOutTimedFastLogger $ \lgr -> runTimedLogT lvl lgr (runReaderT f e)

main :: IO ()
main = withStdOutTimedFastLogger $ \lgr -> do
  let appEnv = E.AppEnv (E.Logger lgr LevelInfo)
  runServiceTimedFastLogger appEnv LevelInfo . unService $ do
    logInfo "Hello world"
    return ()
  pushLogMessage lgr LevelError ("Exiting" :: String)
