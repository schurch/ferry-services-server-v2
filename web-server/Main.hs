{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Base                 (SomeException)
import           Control.Monad.Reader                   (runReaderT)
import           Data.Aeson                             (Value (String))
import           Data.ByteString.Char8                  (unpack)
import           Data.Text.Encoding                     (decodeUtf8)
import           Network.Wai                            (Middleware, Request,
                                                         rawPathInfo,
                                                         rawQueryString,
                                                         requestHeaderHost,
                                                         requestMethod)
import           Network.Wai.Handler.Warp               (Settings,
                                                         defaultOnException,
                                                         defaultSettings,
                                                         setOnException,
                                                         setPort)
import           Network.Wai.Middleware.RequestLogger   (mkRequestLogger)
import           System.Environment                     (getEnv)
import           System.Log.Raven                       (initRaven, register,
                                                         silentFallback)
import           System.Log.Raven.Transport.HttpConduit (sendRecord)
import           System.Log.Raven.Types                 (SentryLevel (Error),
                                                         SentryRecord (..))
import           System.Logger                          (Level (..), Logger,
                                                         Output (StdOut),
                                                         create, info)
import           System.Logger.Message                  (msg)
import           Web.Scotty.Trans                       (Options (Options, settings, verbose),
                                                         scottyOptsT)

import           Types                                  (Env (Env))
import           WebServer                              (loggerSettings, webApp)

import qualified Data.HashMap.Strict                    as HM

main :: IO ()
main = do
  logger <- create StdOut
  port   <- getEnv "SERVER_PORT"
  info logger (msg $ "Listening on port " <> port)
  let settings =
        setPort (read port) . setOnException exceptionHandler $ defaultSettings
  let options = Options { verbose = 0, settings = settings }
  requestLogger <- mkRequestLogger $ loggerSettings logger
  scottyOptsT options (`runReaderT` Env logger) (webApp requestLogger)

exceptionHandler :: Maybe Request -> SomeException -> IO ()
exceptionHandler request exception = do
  sentryDSN     <- getEnv "SERVER_SENTRY_DSN"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  env           <- getEnv "ENVIRONMENT"
  register sentryService
           "api-logger"
           System.Log.Raven.Types.Error
           (show exception)
           (recordUpdate env request exception)
  defaultOnException request exception

recordUpdate
  :: String -> Maybe Request -> SomeException -> SentryRecord -> SentryRecord
recordUpdate _   Nothing        exception record = record
recordUpdate env (Just request) exception record = record
  { srServerName  = Data.ByteString.Char8.unpack <$> requestHeaderHost request
  , srEnvironment = Just env
  , srExtra       = HM.fromList
                      [ ("method", String $ decodeUtf8 $ requestMethod request)
                      , ("path", String $ decodeUtf8 $ rawPathInfo request)
                      , ("query-string", String $ decodeUtf8 $ rawQueryString request)
                      ]
  }
