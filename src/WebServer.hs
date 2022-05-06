{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module WebServer where

import           Control.Monad                        (forM, void, when)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Reader                 (asks)
import           Control.Monad.Trans                  (lift)
import           Data.Aeson                           (Value (..))
import           Data.Char                            (ord)
import           Data.Default                         (def)
import           Data.Maybe                           (fromJust, fromMaybe,
                                                       isNothing)
import           Data.Pool                            (withResource)
import           Data.Scientific                      (Scientific (..),
                                                       fromFloatDigits,
                                                       toRealFloat)
import           Data.Text.Lazy                       (Text, toStrict)
import           Data.Time.Clock                      (UTCTime, diffUTCTime,
                                                       getCurrentTime)
import           Data.UUID                            (UUID, fromText)
import           Database.Postgis                     (Geometry (GeoPoint),
                                                       Point (Point),
                                                       Position (Position))
import           Database.PostgreSQL.Simple           (Connection)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.AddHeaders    (addHeaders)
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip          (GzipFiles (..), def,
                                                       gzip, gzipFiles)
import           Network.Wai.Middleware.RequestLogger (Destination (Callback),
                                                       IPAddrSource (FromFallback),
                                                       OutputFormat (Apache),
                                                       RequestLoggerSettings (destination, outputFormat))
import           Network.Wai.Middleware.Static        (addBase, isNotAbsolute,
                                                       noDots, staticPolicy)
import           System.Log.FastLogger.Internal       (LogStr, fromLogStr)
import           System.Logger                        (Level (Debug, Info, Trace),
                                                       Logger, level, log, msg)
import           System.Logger.Message                (msg)
import           Web.Scotty.Trans                     (Parsable (parseParam),
                                                       delete, file, get, json,
                                                       jsonData, middleware,
                                                       param, post, redirect,
                                                       setHeader)

import           AWS
import           Types

import qualified Data.ByteString                      as BS
import qualified Data.Char                            as Char
import qualified Data.Map                             as M
import qualified Database                             as DB

webApp :: Middleware -> Scotty
webApp requestLogger = do
  middleware requestLogger
  let corsOrigins = ["https://scottishferryapp.com", "https://www.scottishferryapp.com", "http://localhost:3000"]
  middleware $ cors (const $ Just simpleCorsResourcePolicy { corsOrigins = Just (corsOrigins, False) })
  middleware $ gzip def { gzipFiles = GzipCompress }
  middleware $ addHeaders [("X-Frame-Options", "DENY")
                          ,("X-Content-Type-Options", "nosniff")
                          ,("Content-Security-Policy", "script-src 'self' https:, object-src 'none'; base-uri 'none';")]
  middleware $ staticPolicy (noDots <> isNotAbsolute <> addBase "public")
  get "/" $ do
    setHeader "Content-Type" "text/html"
    file "public/index.html"
  get "/api/services" $ do
    services <- getServices
    json services
  get "/api/services/:serviceID" $ do
    serviceID <- param "serviceID"
    service   <- getService serviceID
    json service
  post "/api/installations/:installationID" $ do
    installationID <- param "installationID"
    request        <- jsonData
    services       <- createInstallation installationID request
    json services
  get "/api/installations/:installationID/services" $ do
    installationID <- param "installationID"
    services       <- getServicesForInstallation installationID
    json services
  post "/api/installations/:installationID/services" $ do
    installationID <- param "installationID"
    (AddServiceRequest serviceID) <- jsonData
    services <- addServiceToInstallation installationID serviceID
    json services
  delete "/api/installations/:installationID/services/:serviceID" $ do
    installationID <- param "installationID"
    serviceID      <- param "serviceID"
    services       <- deleteServiceForInstallation installationID serviceID
    json services
  get "/api/vessels" $ do
    vessels <- getVessels
    json vessels

instance Parsable UUID where
  parseParam = maybeToEither "Error parsing UUID" . fromText . toStrict

maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither errorMessage Nothing      = Left errorMessage
maybeToEither _            (Just value) = Right value

loggerSettings :: Logger -> RequestLoggerSettings
loggerSettings logger = case level logger of
  Trace -> debugSettings
  Debug -> debugSettings
  _     -> infoSettings
 where
  debugSettings :: RequestLoggerSettings
  debugSettings = def { destination = Callback (callbackLog Debug) }

  infoSettings :: RequestLoggerSettings
  infoSettings =
    def { outputFormat = Apache FromFallback, destination = Callback (callbackLog Info) }

  callbackLog ::  Level -> LogStr -> IO ()
  callbackLog level str =
    System.Logger.log logger level $ msg $ (removeTrailingNewline . fromLogStr) str

  removeTrailingNewline :: BS.ByteString -> BS.ByteString
  removeTrailingNewline =
    BS.reverse . BS.dropWhile (== fromIntegral (ord '\n')) . BS.reverse

-- Lookup locations for a service ID
type ServiceLocationLookup = M.Map Int [LocationResponse]

-- Lookup vessels for a service ID
type ServiceVesselLookup = M.Map Int [VesselResponse]

getService :: Int -> Action (Maybe ServiceResponse)
getService serviceID = do
  service        <- lift $ DB.getService serviceID
  time           <- liftIO getCurrentTime
  locationLookup <- getLocationLookup
  vesselLookup   <- getServiceVesselLookup
  return $ serviceToServiceResponseWithLocationLookup vesselLookup locationLookup 1 time <$> service

getServices :: Action [ServiceResponse]
getServices = do
  services       <- lift DB.getServices
  time           <- liftIO getCurrentTime
  locationLookup <- getLocationLookup
  vesselLookup   <- getServiceVesselLookup
  forM (zip [1..] services) $ \(sortOrder, service) ->
    return $ serviceToServiceResponseWithLocationLookup vesselLookup locationLookup sortOrder time service

createInstallation
  :: UUID -> CreateInstallationRequest -> Action [ServiceResponse]
createInstallation installationID (CreateInstallationRequest deviceToken deviceType)
  = do
    awsSNSEndpointARN <- registerDeviceToken installationID
                                             deviceToken
                                             deviceType
    time <- liftIO getCurrentTime
    lift $ DB.createInstallation installationID
                          deviceToken
                          deviceType
                          awsSNSEndpointARN
                          time
    getServicesForInstallation installationID

addServiceToInstallation :: UUID -> Int -> Action [ServiceResponse]
addServiceToInstallation installationID serviceID = do
  lift $ DB.addServiceToInstallation installationID serviceID
  getServicesForInstallation installationID

deleteServiceForInstallation :: UUID -> Int -> Action [ServiceResponse]
deleteServiceForInstallation installationID serviceID = do
  lift $ DB.deleteServiceForInstallation installationID serviceID
  getServicesForInstallation installationID

getServicesForInstallation :: UUID -> Action [ServiceResponse]
getServicesForInstallation installationID = do
  services       <- lift $ DB.getServicesForInstallation installationID
  time           <- liftIO getCurrentTime
  locationLookup <- getLocationLookup
  vesselLookup   <- getServiceVesselLookup
  forM (zip [1..] services) $ \(sortOrder, service) ->
    return $ serviceToServiceResponseWithLocationLookup vesselLookup locationLookup sortOrder time service

getVessels :: Action [VesselResponse]
getVessels = do
  time <- liftIO getCurrentTime
  vessels <- filter (vesselFilter time) <$> lift DB.getVessels
  return $ vesselToVesselResponse <$> vessels

vesselFilter :: UTCTime -> Vessel -> Bool
vesselFilter currentTime Vessel { vesselLastReceived = vesselLastReceived } =
  vesselTimeFilter currentTime vesselLastReceived

serviceVesselFilter :: UTCTime -> ServiceVessel -> Bool
serviceVesselFilter currentTime ServiceVessel { serviceVesselLastReceived = vesselLastReceived} =
  vesselTimeFilter currentTime vesselLastReceived

vesselTimeFilter :: UTCTime -> UTCTime -> Bool
vesselTimeFilter currentTime lastReceived =
  let diff = diffUTCTime currentTime lastReceived
  in diff < 1800

vesselToVesselResponse :: Vessel -> VesselResponse
vesselToVesselResponse Vessel {..} =
  VesselResponse
  { vesselResponseMmsi = vesselMmsi
  , vesselResponseName = vesselName
  , vesselResponseSpeed = vesselSpeed
  , vesselResponseCourse = vesselCourse
  , vesselResponseLatitude = getLatitude vesselCoordinate
  , vesselResponseLongitude = getLongitude vesselCoordinate
  , vesselResponseLastReceived = vesselLastReceived
  }
  where
    getLatitude :: Geometry -> Scientific
    getLatitude (GeoPoint _ (Point (Position latitude _ _ _))) = fromFloatDigits latitude
    getLatitude _ = error "Expected point"

    getLongitude :: Geometry -> Scientific
    getLongitude (GeoPoint _ (Point (Position _ longitude _ _))) = fromFloatDigits longitude
    getLongitude _ = error "Expected point"

serviceToServiceResponseWithLocationLookup
  ::  ServiceVesselLookup -> ServiceLocationLookup -> Int -> UTCTime -> Service -> ServiceResponse
serviceToServiceResponseWithLocationLookup vesselLookup locationLookup sortOrder currentTime Service {..}
  = ServiceResponse
    { serviceResponseServiceID        = serviceID
    , serviceResponseSortOrder        = sortOrder
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
    , serviceResponseVessels          = fromMaybe []
                                          $ M.lookup serviceID vesselLookup
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
  storedInstallation <- lift $ DB.getInstallationWithID installationID
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
  serviceLocations <- lift DB.getServiceLocations
  locationWeatherLookup <- getLocationWeatherLookup
  return
    $ M.fromListWith (++)
    $ [ (serviceID, [LocationResponse locationID name (fromFloatDigits latitude) (fromFloatDigits longitude) (M.lookup locationID locationWeatherLookup)])
      | (ServiceLocation serviceID locationID name (GeoPoint _ (Point (Position latitude longitude _ _)))) <-
        serviceLocations
      ]

getServiceVesselLookup :: Action ServiceVesselLookup
getServiceVesselLookup = do
  serviceVessels <- lift $ DB.getServiceVessels
  time <- liftIO getCurrentTime
  vessels <- filter (vesselFilter time) <$> lift DB.getVessels
  return
    $ M.fromListWith (++)
    $ [ (serviceID, [vesselToVesselResponse (Vessel mmsi name speed course coordinate lastReceived updated)])
      | vessel@(ServiceVessel serviceID mmsi name speed course coordinate lastReceived updated) <- serviceVessels
      , serviceVesselFilter time vessel
      ]

getLocationWeatherLookup :: Action (M.Map Int LocationWeatherResponse)
getLocationWeatherLookup = do
  locationWeathers <- lift DB.getLocationWeathers
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
    capitalized []       = []
