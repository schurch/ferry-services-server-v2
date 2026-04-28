{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import App.Runner
  ( SentryConfig (SentryConfig),
    createEnv,
    handleSentryException,
  )
import App.Logger
  ( Logger,
    logInfo,
  )
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, throwIO)
import Control.Monad (forever)
import Control.Monad.Trans.Reader (runReaderT)
import System.Environment (getArgs)
import TransxchangeV2.Ingest
  ( ingestDirectoryV2,
    ingestLatestV2,
  )
import TransxchangeV2.Types (Tx2IngestSummary (..))
import Types (Env (Env))

main :: IO ()
main = do
  args <- getArgs
  env@(Env logger _) <- createEnv
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
handleException =
  handleSentryException $
    SentryConfig
      "TRANSXCHANGE_INGESTER_SENTRY_DSN"
      "transxchange-ingester-v2-logger"
