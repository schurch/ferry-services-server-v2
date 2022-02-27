{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Control.Concurrent                     (threadDelay)
import           Control.Exception                      (SomeException, catch)
import           Control.Monad                          (forM_, forever, void,
                                                         when)
import           System.Environment                     (getEnv)
import           System.Log.Raven                       (initRaven, register,
                                                         silentFallback)
import           System.Log.Raven.Transport.HttpConduit (sendRecord)
import           System.Log.Raven.Types                 (SentryLevel (Error),
                                                         SentryRecord (..))
import           System.Logger                          (Logger,
                                                         Output (StdOut),
                                                         create, err, info)
import           System.Logger.Class                    (Logger, debug)
import           System.Logger.Message                  (msg)

import           Scraper

main :: IO ()
main = do
  logger <- create StdOut
  forever $ do
    info logger (msg @String "Fetching statuses")
    catch (fetchCalMacStatusesAndNotify logger) (handleException logger)
    catch (fetchNorthLinkServicesAndNotify logger) (handleException logger)
    threadDelay (15 * 60 * 1000 * 1000) -- 15 mins

handleException :: Logger -> SomeException -> IO ()
handleException logger exception = do
  err logger (msg $ "An error occured: " <> show exception)
  sentryDSN     <- getEnv "SCRAPER_SENTRY_DSN"
  env           <- getEnv "ENVIRONMENT"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  register sentryService
           "scraper-logger"
           Error
           (show exception)
           (recordUpdate env)

recordUpdate :: String -> SentryRecord -> SentryRecord
recordUpdate env record = record { srEnvironment = Just env }
