{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (SomeException, catch, throwIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Pool (createPool)
import Data.String (fromString)
import Database.PostgreSQL.Simple
  ( close,
    connectPostgreSQL,
  )
import System.Environment (getArgs, getEnv)
import System.Log.Raven
  ( initRaven,
    register,
    silentFallback,
  )
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types
  ( SentryLevel (Error),
    SentryRecord (..),
  )
import System.Logger
  ( Logger,
    Output (StdOut),
    create,
    err,
    info,
  )
import System.Logger.Message (msg)
import TransxchangeV2.Ingest
  ( ingestDirectoryV2,
    ingestLatestV2,
  )
import TransxchangeV2.Types (Tx2IngestSummary (..))
import Types (Env (Env))

main :: IO ()
main = do
  args <- getArgs
  logger <- create StdOut
  connectionString <- getEnv "DB_CONNECTION"
  connectionPool <-
    createPool
      (connectPostgreSQL $ fromString connectionString)
      Database.PostgreSQL.Simple.close
      2
      60
      10
  let env = Env logger connectionPool
  catch
    (do
        (summary, sourceLabel) <-
          case args of
            (directory : _) -> do
              result <- runReaderT (ingestDirectoryV2 directory) env
              return (result, directory)
            [] -> do
              result <- runReaderT ingestLatestV2 env
              return (result, "FTP S.zip")
        info logger (msg @String $ "TransXChange v2 ingest complete from: " <> sourceLabel)
        info logger (msg @String $ renderSummary summary)
    )
    (\exception -> do
        handleException logger exception
        throwIO exception
    )

renderSummary :: Tx2IngestSummary -> String
renderSummary summary =
  "files_parsed="
    <> show (tx2FilesParsed summary)
    <> ", files_skipped="
    <> show (tx2FilesSkipped summary)
    <> ", files_failed="
    <> show (tx2FilesFailed summary)
    <> ", ferry_documents="
    <> show (tx2FerryDocuments summary)
    <> ", services_written="
    <> show (tx2ServicesWritten summary)
    <> ", vehicle_journeys_written="
    <> show (tx2JourneyWritten summary)

handleException :: Logger -> SomeException -> IO ()
handleException logger exception = do
  err logger (msg $ "An error occured: " <> show exception)
  sentryDSN <- getEnv "TRANSXCHANGE_INGESTER_SENTRY_DSN"
  env <- getEnv "ENVIRONMENT"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  register
    sentryService
    "transxchange-ingester-v2-logger"
    Error
    (show exception)
    (recordUpdate env)

recordUpdate :: String -> SentryRecord -> SentryRecord
recordUpdate env record = record {srEnvironment = Just env}
