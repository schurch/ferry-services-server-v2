{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Control.Concurrent                     (threadDelay)
import           Control.Exception                      (SomeException, catch)
import           Control.Monad                          (forever)
import           System.Environment                     (getEnv)
import           System.Log.Raven                       (initRaven, register,
                                                         silentFallback)
import           System.Log.Raven.Transport.HttpConduit (sendRecord)
import           System.Log.Raven.Types                 (SentryLevel (Error),
                                                         SentryRecord (..))
import           System.Logger                          (Logger,
                                                         Output (StdOut),
                                                         create, debug, err,
                                                         info)
import           System.Logger.Message                  (msg)

import           VesselFetcher                          (defaultMmsis,
                                                         fetchVessels)

main :: IO ()
main = do
  logger <- create StdOut
  forever $ do
    info logger (msg @String "Fetching vessels")
    catch (fetchVessels logger defaultMmsis) (handleException logger)
    threadDelay (10 * 60 * 1000 * 1000) -- 15 mins

handleException :: Logger -> SomeException -> IO ()
handleException logger exception = do
  err logger (msg $ "An error occured: " <> show exception)
  sentryDSN     <- getEnv "VESSEL_FETCHER_SENTRY_DSN"
  env           <- getEnv "ENVIRONMENT"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  register sentryService
           "vessel-fetcher-logger"
           Error
           (show exception)
           (recordUpdate env)

recordUpdate :: String -> SentryRecord -> SentryRecord
recordUpdate env record = record { srEnvironment = Just env }
