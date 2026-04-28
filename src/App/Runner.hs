{-# LANGUAGE OverloadedStrings #-}

module App.Runner
  ( SentryConfig (..),
    createEnv,
    handleSentryException,
    runRepeatedAction,
    runRepeatedActions,
  )
where

import App.Database (createConnectionPool)
import App.Logger
  ( Logger,
    Output (StdOut),
    create,
    logError,
    logInfo,
  )
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
import Types (Application, Env (Env))

data SentryConfig = SentryConfig
  { sentryDsnEnvVar :: String,
    sentryLoggerName :: String
  }

createEnv :: IO Env
createEnv = do
  logger <- create StdOut
  connectionString <- getEnv "DB_CONNECTION"
  connectionPool <- createConnectionPool connectionString
  pure $ Env logger connectionPool

handleSentryException :: SentryConfig -> Logger -> SomeException -> IO ()
handleSentryException config logger exception = do
  logError logger $ "An error occured: " <> show exception
  sentryDSN <- getEnv $ sentryDsnEnvVar config
  environment <- getEnv "ENVIRONMENT"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  register
    sentryService
    (sentryLoggerName config)
    Error
    (show exception)
    (recordUpdate environment)

runRepeatedAction :: SentryConfig -> String -> Int -> Application () -> IO ()
runRepeatedAction config label sleepMicros action =
  runRepeatedActions config label sleepMicros [action]

runRepeatedActions :: SentryConfig -> String -> Int -> [Application ()] -> IO ()
runRepeatedActions config label sleepMicros actions = do
  env@(Env logger _) <- createEnv
  forever $ do
    logInfo logger label
    mapM_ (runAction logger env) actions
    threadDelay sleepMicros
  where
    runAction logger env action =
      catch
        (runReaderT action env)
        (handleSentryException config logger)

recordUpdate :: String -> SentryRecord -> SentryRecord
recordUpdate environment record = record {srEnvironment = Just environment}
