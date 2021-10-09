{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Lib
import           Types
import           Web.Scotty.Trans
import           Control.Monad.Reader           ( runReaderT )
import           Control.Monad.IO.Class         ( liftIO )
import           Network.Wai.Handler.Warp       ( Settings
                                                , defaultOnException
                                                , defaultSettings
                                                , setOnException
                                                , setPort
                                                )
import           Network.Wai                    ( Request
                                                , Middleware
                                                , rawPathInfo
                                                , rawQueryString
                                                , requestHeaderHost
                                                , requestMethod
                                                )
import           Network.Wai.Middleware.RequestLogger
import           Control.Exception.Base         ( SomeException )
import           System.Log.Raven               ( initRaven
                                                , register
                                                , silentFallback
                                                )
import           System.Log.Raven.Transport.HttpConduit
                                                ( sendRecord )
import           System.Log.Raven.Types         ( SentryLevel(Error)
                                                , SentryRecord(..)
                                                )
import           Data.ByteString.Char8          ( unpack )
import           Data.Aeson                     ( Value(..) )
import           Data.Text.Encoding             ( decodeUtf8 )
import           System.Environment             ( getEnv )
import           System.Logger                  ( create
                                                , Output(StdOut)
                                                , info
                                                , Logger
                                                , Level(..)
                                                , level
                                                , log
                                                )
import           System.Logger.Message          ( msg )
import           Data.UUID                      ( UUID
                                                , fromText
                                                )
import           Data.Text.Lazy                 ( Text
                                                , toStrict
                                                , unpack
                                                )
import           Data.Default                   ( def )
import           System.Log.FastLogger.Internal ( LogStr
                                                , fromLogStr
                                                )
import           Network.Wai.Middleware.Static  ( staticPolicy
                                                , noDots
                                                , isNotAbsolute
                                                , addBase
                                                )
import           Data.Time.Calendar             ( Day )
import           Data.Char                      ( ord )

import           Data.ByteString               as BS
import qualified Data.HashMap.Strict           as HM

main :: IO ()
main = do
  logger <- create StdOut
  port   <- getEnv "SERVER_PORT"
  info logger (msg $ "Listening on port " <> port <> "...")
  let settings =
        setPort (read port) . setOnException exceptionHandler $ defaultSettings
  let options = Options { verbose = 0, settings = settings }
  requestLogger <- mkRequestLogger $ loggerSettings logger
  scottyOptsT options (`runReaderT` Env logger) (app requestLogger)

app :: Middleware -> Scotty
app requestLogger = do
  middleware requestLogger
  middleware $ staticPolicy (noDots <> isNotAbsolute <> addBase "public")
  get "/" $ redirect "/index.html"
  get "/api/services" $ do
    services <- getServices
    setHeader "Access-Control-Allow-Origin" "*"
    json services
  get "/api/services/:serviceID" $ do
    serviceID <- param "serviceID"
    service   <- getService serviceID
    setHeader "Access-Control-Allow-Origin" "*"
    json service
  post "/api/installations/:installationID" $ do
    installationID <- param "installationID"
    request        <- jsonData
    services       <- createInstallation installationID request
    setHeader "Access-Control-Allow-Origin" "*"
    json services
  get "/api/installations/:installationID/services" $ do
    installationID <- param "installationID"
    services       <- getServicesForInstallation installationID
    setHeader "Access-Control-Allow-Origin" "*"
    json services
  post "/api/installations/:installationID/services" $ do
    installationID <- param "installationID"
    (AddServiceRequest serviceID) <- jsonData
    services <- addServiceToInstallation installationID serviceID
    setHeader "Access-Control-Allow-Origin" "*"
    json services
  delete "/api/installations/:installationID/services/:serviceID" $ do
    installationID <- param "installationID"
    serviceID      <- param "serviceID"
    services       <- deleteServiceForInstallation installationID serviceID
    setHeader "Access-Control-Allow-Origin" "*"
    json services

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

instance Parsable UUID where
  parseParam = maybeToEither "Error parsing UUID" . fromText . toStrict

maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither errorMessage Nothing      = Left errorMessage
maybeToEither _            (Just value) = Right value

loggerSettings :: Logger -> RequestLoggerSettings
loggerSettings logger = case currentLogLevel of
  Trace               -> debugSettings
  Debug               -> debugSettings
  Info                -> infoSettings
  Warn                -> infoSettings
  System.Logger.Error -> infoSettings
  Fatal               -> infoSettings
 where
  debugSettings :: RequestLoggerSettings
  debugSettings = def { destination = Callback callbackLog }

  infoSettings :: RequestLoggerSettings
  infoSettings =
    def { outputFormat = Apache FromSocket, destination = Callback callbackLog }

  currentLogLevel :: Level
  currentLogLevel = level logger

  callbackLog :: LogStr -> IO ()
  callbackLog str =
    System.Logger.log logger currentLogLevel
      $ msg
      $ (removeTrailingNewline . fromLogStr) str

  removeTrailingNewline :: BS.ByteString -> BS.ByteString
  removeTrailingNewline =
    BS.reverse . BS.dropWhile (== fromIntegral (ord '\n')) . BS.reverse
