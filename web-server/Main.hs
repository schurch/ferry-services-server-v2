{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Exception.Base         ( SomeException )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader           ( runReaderT
                                                , asks
                                                )
import           Data.Aeson                     ( Value(..) )
import           Data.ByteString.Char8          ( unpack )
import           Data.Char                      ( ord )
import           Data.Default                   ( def )
import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                , fromJust
                                                )
import           Data.Scientific                ( Scientific(..)
                                                , toRealFloat 
                                                )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Text.Lazy                 ( Text
                                                , toStrict
                                                , unpack
                                                )
import           Data.Time.Calendar             ( Day )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                , diffUTCTime
                                                )
import           Data.UUID                      ( UUID
                                                , fromText
                                                )
import           Network.Wai                    ( Request
                                                , Middleware
                                                , rawPathInfo
                                                , rawQueryString
                                                , requestHeaderHost
                                                , requestMethod
                                                )
import           Network.Wai.Handler.Warp       ( Settings
                                                , defaultOnException
                                                , defaultSettings
                                                , setOnException
                                                , setPort
                                                )
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static  ( staticPolicy
                                                , noDots
                                                , isNotAbsolute
                                                , addBase
                                                )
import           System.Environment             ( getEnv )
import           System.Log.FastLogger.Internal ( LogStr
                                                , fromLogStr
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
import           System.Logger                  ( create
                                                , Output(StdOut)
                                                , info
                                                , Logger
                                                , Level(..)
                                                , level
                                                , log
                                                )
import           System.Logger.Message          ( msg )

import           Web.Scotty.Trans
import           Types
import           AWS

import qualified Data.ByteString               as BS
import qualified Data.Char                     as Char
import qualified Data.HashMap.Strict           as HM
import qualified Data.Map                      as M
import qualified Database                      as DB

main :: IO ()
main = do
  logger <- create StdOut
  port   <- getEnv "SERVER_PORT"
  info logger (msg $ "Listening on port " <> port)
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
    def { outputFormat = Apache FromFallback, destination = Callback callbackLog }

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

-- Lookup locations for a service ID
type ServiceLocationLookup = M.Map Int [LocationResponse]

getService :: Int -> Action (Maybe ServiceResponse)
getService serviceID = do
  service        <- DB.getService serviceID
  time           <- liftIO getCurrentTime
  locationLookup <- getLocationLookup
  return
    $   serviceToServiceResponseWithLocationLookup locationLookup time
    <$> service

getServices :: Action [ServiceResponse]
getServices = do
  services       <- DB.getServices
  time           <- liftIO getCurrentTime
  locationLookup <- getLocationLookup
  return
    $   serviceToServiceResponseWithLocationLookup locationLookup time
    <$> services

createInstallation
  :: UUID -> CreateInstallationRequest -> Action [ServiceResponse]
createInstallation installationID (CreateInstallationRequest deviceToken deviceType)
  = do
    awsSNSEndpointARN <- registerDeviceToken installationID
                                             deviceToken
                                             deviceType
    time <- liftIO getCurrentTime
    DB.createInstallation installationID
                          deviceToken
                          deviceType
                          awsSNSEndpointARN
                          time
    getServicesForInstallation installationID

addServiceToInstallation :: UUID -> Int -> Action [ServiceResponse]
addServiceToInstallation installationID serviceID = do
  DB.addServiceToInstallation installationID serviceID
  getServicesForInstallation installationID

deleteServiceForInstallation :: UUID -> Int -> Action [ServiceResponse]
deleteServiceForInstallation installationID serviceID = do
  DB.deleteServiceForInstallation installationID serviceID
  getServicesForInstallation installationID

getServicesForInstallation :: UUID -> Action [ServiceResponse]
getServicesForInstallation installationID = do
  services       <- DB.getServicesForInstallation installationID
  time           <- liftIO getCurrentTime
  locationLookup <- getLocationLookup
  return
    $   serviceToServiceResponseWithLocationLookup locationLookup time
    <$> services

serviceToServiceResponseWithLocationLookup
  :: ServiceLocationLookup -> UTCTime -> Service -> ServiceResponse
serviceToServiceResponseWithLocationLookup locationLookup currentTime Service {..}
  = ServiceResponse
    { serviceResponseServiceID        = serviceID
    , serviceResponseSortOrder        = serviceSortOrder
    , serviceResponseArea             = serviceArea
    , serviceResponseRoute            = serviceRoute
    , serviceResponseStatus           = serviceStatusForTime currentTime
                                                             serviceUpdated
                                                             serviceStatus
    , serviceResponseLocations        = fromMaybe []
                                          $ M.lookup serviceID locationLookup
    , serviceResponseAdditionalInfo   = serviceAdditionalInfo
    , serviceResponseDisruptionReason = serviceDisruptionReason
    , serviceResponseLastUpdatedDate  = serviceLastUpdatedDate
    , serviceResponseUpdated          = serviceUpdated
    }

-- Unknown status if over 30 mins ago
serviceStatusForTime :: UTCTime -> UTCTime -> ServiceStatus -> ServiceStatus
serviceStatusForTime currentTime serviceUpdated serviceStatus =
  let diff = diffUTCTime currentTime serviceUpdated
  in  if diff > 1800 then Unknown else serviceStatus

-- Pseudo code from AWS docs:
-- retrieve the latest device token from the mobile operating system
-- if (the platform endpoint ARN is not stored)
--   # this is a first-time registration
--   call create platform endpoint
--   store the returned platform endpoint ARN
-- endif

-- call get endpoint attributes on the platform endpoint ARN 

-- if (while getting the attributes a not-found exception is thrown)
--   # the platform endpoint was deleted 
--   call create platform endpoint with the latest device token
--   store the returned platform endpoint ARN
-- else 
--   if (the device token in the endpoint does not match the latest one) or 
--       (get endpoint attributes shows the endpoint as disabled)
--     call set endpoint attributes to set the latest device token and then enable the platform endpoint
--   endif
-- endif
registerDeviceToken :: UUID -> String -> DeviceType -> Action String
registerDeviceToken installationID deviceToken deviceType = do
  logger'            <- asks logger
  storedInstallation <- DB.getInstallationWithID installationID
  currentEndpointARN <- if isNothing storedInstallation
    then liftIO $ createPushEndpoint logger' deviceToken deviceType
    else return $ installationEndpointARN . fromJust $ storedInstallation
  endpointAttributesResult <- liftIO
    $ getAttributesForEndpoint logger' currentEndpointARN
  case endpointAttributesResult of
    EndpointAttributesEndpointNotFound ->
      liftIO $ createPushEndpoint logger' deviceToken deviceType
    AttributeResults awsDeviceToken isEnabled -> do
      when (awsDeviceToken /= deviceToken || not isEnabled)
        $ liftIO
        . void
        $ updateDeviceTokenForEndpoint logger' currentEndpointARN deviceToken
      return currentEndpointARN

getLocationLookup :: Action ServiceLocationLookup
getLocationLookup = do
  serviceLocations <- liftIO DB.getServiceLocations
  locationWeatherLookup <- getLocationWeatherLookup
  return
    $ M.fromListWith (++)
    $ [ (serviceID, [LocationResponse locationID name latitude longitude (M.lookup locationID locationWeatherLookup)])
      | (ServiceLocation serviceID locationID name latitude longitude) <-
        serviceLocations
      ]

getLocationWeatherLookup :: Action (M.Map Int LocationWeatherResponse)
getLocationWeatherLookup = do
  locationWeathers <- liftIO DB.getLocationWeathers
  return
    $ M.fromList
    $ [ (locationID
        , LocationWeatherResponse icon (capitalized description) (kelvinToCelsius temperature) (metersPerSecondToMilesPerHour windSpeed) windDirection (directionToCardinal windDirection)
        )
      | (LocationWeather locationID description icon temperature windSpeed windDirection _ _) <-
        locationWeathers
      ]
  where
    kelvinToCelsius :: Scientific -> Int
    kelvinToCelsius kelvin =  round . toRealFloat $ kelvin - 273.15

    metersPerSecondToMilesPerHour :: Scientific -> Int
    metersPerSecondToMilesPerHour mps =  round . toRealFloat $ mps * 2.236936284

    directionToCardinal :: Scientific -> String
    directionToCardinal degrees =  
      let 
        index = floor ((toRealFloat degrees + 11.25) / 22.5)
        cardinals = ["N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"]
      in cardinals !! (index `mod` 16)

    capitalized :: String -> String
    capitalized (x : xs) = Char.toUpper x : map Char.toLower xs
    capitalized [] = []
