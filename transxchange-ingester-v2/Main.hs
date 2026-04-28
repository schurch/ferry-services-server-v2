{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (SomeException, catch, throwIO)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Trans.Reader (runReaderT)
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
import App.Logger
  ( Logger,
    Output (StdOut),
    create,
    logError,
    logInfo,
  )
import App.Database (createConnectionPool)
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
  connectionPool <- createConnectionPool connectionString
  let env = Env logger connectionPool
  case args of
    (directory : _) ->
      catch
        (runOnce logger env directory)
        (\exception -> do
            handleException logger exception
            throwIO exception
        )
    [] ->
      forever $ do
        catch
          (runLatest logger env)
          (handleException logger)
        threadDelay (1 * 24 * 60 * 60 * 1000 * 1000)

runOnce :: Logger -> Env -> FilePath -> IO ()
runOnce logger env directory = do
  summary <- runReaderT (ingestDirectoryV2 directory) env
  logInfo logger $ "TransXChange v2 ingest complete from: " <> directory
  logInfo logger $ renderSummary summary

runLatest :: Logger -> Env -> IO ()
runLatest logger env = do
  summary <- runReaderT ingestLatestV2 env
  logInfo logger "TransXChange v2 ingest complete from: FTP S.zip"
  logInfo logger $ renderSummary summary

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
  logError logger $ "An error occured: " <> show exception
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
