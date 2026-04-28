{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import Control.Monad.Trans.Reader (runReaderT)
import System.Environment (getEnv)
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
import Types
import VesselFetcher
  ( defaultMmsis,
    fetchVessels,
  )

main :: IO ()
main = do
  logger <- create StdOut
  connectionString <- getEnv "DB_CONNECTION"
  connectionPool <- createConnectionPool connectionString
  let env = Env logger connectionPool
  forever $ do
    logInfo logger "Fetching vessels"
    catch (runReaderT (fetchVessels defaultMmsis) env) (handleException logger)
    threadDelay (5 * 60 * 1000 * 1000) -- 5 mins

handleException :: Logger -> SomeException -> IO ()
handleException logger exception = do
  logError logger $ "An error occured: " <> show exception
  sentryDSN <- getEnv "VESSEL_FETCHER_SENTRY_DSN"
  env <- getEnv "ENVIRONMENT"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  register
    sentryService
    "vessel-fetcher-logger"
    Error
    (show exception)
    (recordUpdate env)

recordUpdate :: String -> SentryRecord -> SentryRecord
recordUpdate env record = record {srEnvironment = Just env}
