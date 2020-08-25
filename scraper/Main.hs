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

main :: IO ()
main = do
  putStrLn "Starting scraper..."
  forever $ do
    putStrLn "Fetching statuses..."
    catch fetchStatusesAndNotify handleException
    threadDelay (900 * 1000 * 1000) -- 15 mins

handleException :: SomeException -> IO ()
handleException exception = do
  putStrLn $ "An error occured: " <> show exception
  sentryDSN     <- getEnv "SCRAPER_SENTRY_DSN"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  register sentryService "scraper-logger" Error (show exception) id
