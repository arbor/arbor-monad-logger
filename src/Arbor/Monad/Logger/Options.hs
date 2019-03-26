module Arbor.Monad.Logger.Options
( LogLevel (..)
, logLevelParser
)
where

import Control.Monad.Logger (LogLevel (..))
import Data.Monoid          ((<>))
import Options.Applicative

logLevelParser :: Parser LogLevel
logLevelParser =
  option auto
    (  long "log-level"
    <> metavar "LOG_LEVEL"
    <> showDefault <> value LevelInfo
    <> help "Log level. Valid values are LevelDebug, LevelInfo, LevelWarn, LevelError"
    )
