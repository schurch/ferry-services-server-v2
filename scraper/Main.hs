module Main where

import           Lib
import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever )
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           System.Log.Raven               ( initRaven
                                                , register
                                                , silentFallback
                                                )
import           System.Log.Raven.Transport.HttpConduit
                                                ( sendRecord )
import           System.Log.Raven.Types         ( SentryLevel(Error)
                                                , SentryRecord(..)
                                                )
import           System.Environment             ( getEnv )
import           System.Logger                  ( Output(StdOut)
                                                , Logger
                                                , create
                                                , info
                                                , err
                                                )
import           System.Logger.Message          ( msg )

main :: IO ()
main = do
  logger <- create StdOut
  info logger (msg "Starting scraper...")
  forever $ do
    info logger (msg "Fetching statuses...")
    catch (fetchStatusesAndNotify logger) (handleException logger)
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
