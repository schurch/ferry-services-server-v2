{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad
  ( forM_,
    forever,
    void,
    when,
  )
import Control.Monad.Trans.Reader (runReaderT)
import Scraper
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

main :: IO ()
main = do
  logger <- create StdOut
  connectionString <- getEnv "DB_CONNECTION"
  connectionPool <- createConnectionPool connectionString
  let env = Env logger connectionPool
  forever $ do
    logInfo logger "Fetching statuses"
    catch (runReaderT fetchCalMacStatusesAndNotify env) (handleException logger)
    catch (runReaderT fetchCorranFerryAndNotify env) (handleException logger)
    catch (runReaderT fetchNorthLinkServicesAndNotify env) (handleException logger)
    catch (runReaderT fetchPentlandFerriesAndNotify env) (handleException logger)
    catch (runReaderT fetchWesternFerriesAndNotify env) (handleException logger)
    catch (runReaderT fetchShetlandFerriesAndNotify env) (handleException logger)
    catch (runReaderT fetchOrkneyFerriesAndNotify env) (handleException logger)
    threadDelay (15 * 60 * 1000 * 1000) -- 15 mins

handleException :: Logger -> SomeException -> IO ()
handleException logger exception = do
  logError logger $ "An error occured: " <> show exception
  sentryDSN <- getEnv "SCRAPER_SENTRY_DSN"
  env <- getEnv "ENVIRONMENT"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  register
    sentryService
    "scraper-logger"
    Error
    (show exception)
    (recordUpdate env)

recordUpdate :: String -> SentryRecord -> SentryRecord
recordUpdate env record = record {srEnvironment = Just env}
