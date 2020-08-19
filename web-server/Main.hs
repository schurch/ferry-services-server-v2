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
import qualified Data.HashMap.Strict           as HM

main :: IO ()
main = do
  let settings =
        setPort 3000 . setOnException exceptionHandler $ defaultSettings
  let options = Options { verbose = 0, settings = settings }
  scottyOpts options $ do
    get "/services" $ do
      services <- liftIO getServices
      Web.Scotty.json services
    get "/services/:serviceID" $ do
      serviceID <- param "serviceID"
      service   <- liftIO $ getServiceDetails serviceID
      Web.Scotty.json service

exceptionHandler :: Maybe Request -> SomeException -> IO ()
exceptionHandler request exception = do
  sentryDSN     <- getEnv "SERVER_SENTRY_DSN"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  register sentryService
           "api-logger"
           Error
           (show exception)
           (recordUpdate request exception)
  defaultOnException request exception

recordUpdate :: Maybe Request -> SomeException -> SentryRecord -> SentryRecord
recordUpdate Nothing        exception record = record
recordUpdate (Just request) exception record = record
  { srServerName = unpack <$> requestHeaderHost request
  , srExtra      = HM.fromList
    [ ("method"      , (String $ decodeUtf8 $ requestMethod request))
    , ("path"        , (String $ decodeUtf8 $ rawPathInfo request))
    , ("query-string", (String $ decodeUtf8 $ rawQueryString request))
    ]
  }
