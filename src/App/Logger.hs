{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.Logger
  ( Level (..),
    Logger,
    Output (..),
    MonadLogger (..),
    create,
    level,
    logMessage,
    logTrace,
    logDebug,
    logInfo,
    logError,
    logTraceM,
    logDebugM,
    logInfoM,
    logErrorM,
    close,
    ToLogMessage (..),
  )
where

import qualified Data.ByteString as BS
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Environment (lookupEnv)
import System.Log.FastLogger
  ( LoggerSet,
    defaultBufSize,
    flushLogStr,
    newStdoutLoggerSet,
    pushLogStr,
    rmLoggerSet,
    toLogStr,
  )

data Level = Trace | Debug | Info | Warn | Error | Fatal
  deriving (Eq, Ord, Read, Show)

data Output = StdOut

data Logger = Logger
  { loggerSet :: LoggerSet,
    loggerLevel :: Level
  }

class Monad m => MonadLogger m where
  askLogger :: m Logger

class ToLogMessage a where
  toLogMessage :: a -> BL.ByteString

instance ToLogMessage String where
  toLogMessage = C.pack

instance ToLogMessage BS.ByteString where
  toLogMessage = BL.fromStrict

instance ToLogMessage BL.ByteString where
  toLogMessage = id

create :: Output -> IO Logger
create StdOut = do
  configuredLevel <- parseLogLevel . fromMaybe "Info" <$> lookupEnv "LOG_LEVEL"
  set <- newStdoutLoggerSet defaultBufSize
  pure $ Logger set configuredLevel

level :: Logger -> Level
level = loggerLevel

logTrace :: Logger -> String -> IO ()
logTrace logger = logMessage logger Trace

logDebug :: Logger -> String -> IO ()
logDebug logger = logMessage logger Debug

logInfo :: Logger -> String -> IO ()
logInfo logger = logMessage logger Info

logError :: Logger -> String -> IO ()
logError logger = logMessage logger Error

logTraceM :: (MonadLogger m, MonadIO m) => String -> m ()
logTraceM = logMessageM Trace

logDebugM :: (MonadLogger m, MonadIO m) => String -> m ()
logDebugM = logMessageM Debug

logInfoM :: (MonadLogger m, MonadIO m) => String -> m ()
logInfoM = logMessageM Info

logErrorM :: (MonadLogger m, MonadIO m) => String -> m ()
logErrorM = logMessageM Error

logMessageM :: (MonadLogger m, MonadIO m) => Level -> String -> m ()
logMessageM messageLevel message = do
  logger <- askLogger
  liftIO $ logMessage logger messageLevel message

logMessage :: ToLogMessage message => Logger -> Level -> message -> IO ()
logMessage logger messageLevel message =
  if level logger > messageLevel
    then pure ()
    else do
      now <- getCurrentTime
      let timestamp = C.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
          rendered =
            timestamp
              <> ", "
              <> levelLabel messageLevel
              <> ", "
              <> toLogMessage message
              <> "\n"
      pushLogStr (loggerSet logger) (toLogStr rendered)

close :: Logger -> IO ()
close logger = do
  flushLogStr (loggerSet logger)
  rmLoggerSet (loggerSet logger)

parseLogLevel :: String -> Level
parseLogLevel value =
  case map toLower value of
    "trace" -> Trace
    "debug" -> Debug
    "info" -> Info
    "warn" -> Warn
    "warning" -> Warn
    "error" -> Error
    "fatal" -> Fatal
    _ -> Info

levelLabel :: Level -> BL.ByteString
levelLabel Trace = "T"
levelLabel Debug = "D"
levelLabel Info = "I"
levelLabel Warn = "W"
levelLabel Error = "E"
levelLabel Fatal = "F"
