{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import           Web.Scotty
import           Control.Monad.IO.Class         ( liftIO )
import           Network.Wai.Handler.Warp       ( Settings
                                                , defaultOnException
                                                , defaultSettings
                                                , setOnException
                                                , setPort
                                                )
import           Network.Wai                    ( Request
                                                , rawPathInfo
                                                , rawQueryString
                                                , requestHeaderHost
                                                , requestMethod
                                                )
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
import           Data.UUID                      ( UUID
                                                , fromText
                                                )
import           Data.Text.Lazy                 ( Text
                                                , toStrict
                                                )

import qualified Data.HashMap.Strict           as HM

main :: IO ()
main = do
  port <- getEnv "SERVER_PORT"
  putStrLn $ "Listening on port " <> port <> "..."
  let settings =
        setPort (read port) . setOnException exceptionHandler $ defaultSettings
  let options = Options { verbose = 0, settings = settings }
  scottyOpts options $ do
    get "/api/services" $ do
      services <- liftIO getServices
      json services
    get "/api/services/:serviceID" $ do
      serviceID <- param "serviceID"
      service   <- liftIO $ getService serviceID
      json service
    post "/api/installations/:installationID" $ do
      installationID <- param "installationID"
      request        <- jsonData
      services       <- liftIO $ createInstallation installationID request
      json services
    get "/api/installations/:installationID/services" $ do
      installationID <- param "installationID"
      services       <- liftIO $ getServicesForInstallation installationID
      json services
    post "/api/installations/:installationID/services" $ do
      installationID <- param "installationID"
      (AddServiceRequest serviceID) <- jsonData
      services <- liftIO $ addServiceToInstallation installationID serviceID
      json services
    delete "/api/installations/:installationID/services/:serviceID" $ do
      installationID <- param "installationID"
      serviceID <- param "serviceID"
      services <- liftIO $ deleteServiceForInstallation installationID serviceID
      json services

exceptionHandler :: Maybe Request -> SomeException -> IO ()
exceptionHandler request exception = do
  sentryDSN     <- getEnv "SERVER_SENTRY_DSN"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  env           <- getEnv "ENVIRONMENT"
  register sentryService
           "api-logger"
           Error
           (show exception)
           (recordUpdate env request exception)
  defaultOnException request exception

recordUpdate
  :: String -> Maybe Request -> SomeException -> SentryRecord -> SentryRecord
recordUpdate _   Nothing        exception record = record
recordUpdate env (Just request) exception record = record
  { srServerName  = unpack <$> requestHeaderHost request
  , srEnvironment = Just env
  , srExtra       = HM.fromList
    [ ("method"      , (String $ decodeUtf8 $ requestMethod request))
    , ("path"        , (String $ decodeUtf8 $ rawPathInfo request))
    , ("query-string", (String $ decodeUtf8 $ rawQueryString request))
    ]
  }

instance Parsable UUID where
  parseParam = maybeToEither "Error parsing UUID" . fromText . toStrict

maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither errorMessage Nothing      = Left errorMessage
maybeToEither _            (Just value) = Right value
