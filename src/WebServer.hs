{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebServer where

import AWS
import Control.Monad (forM, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Data.Aeson (Value (..))
import qualified Data.ByteString as BS
import Data.Char (ord)
import qualified Data.Char as Char
import Data.Default (def)
import Data.List (find, sortOn)
import qualified Data.Map as M
import Data.Maybe
  ( fromJust,
    fromMaybe,
    isNothing,
  )
import Data.Pool (withResource)
import Data.Scientific
  ( Scientific (..),
    fromFloatDigits,
    toRealFloat,
  )
import Data.Text.Lazy (Text, toStrict, unpack)
import Data.Time
  ( LocalTime,
    UTCTime (UTCTime),
    diffUTCTime,
    getCurrentTime,
    localTimeToUTC,
    utctDay,
  )
import Data.Time.Calendar (Day)
import Data.UUID (UUID, fromText)
import qualified Database as DB
import Database.Postgis
  ( Geometry (GeoPoint),
    Point (Point),
    Position (Position),
  )
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types.Status (status404)
import Network.Wai (Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Gzip
  ( GzipFiles (..),
    def,
    gzip,
    gzipFiles,
  )
import Network.Wai.Middleware.RequestLogger
  ( Destination (Callback),
    IPAddrSource (FromFallback),
    OutputFormat (Apache),
    RequestLoggerSettings (destination, outputFormat),
  )
import Network.Wai.Middleware.Static
  ( addBase,
    isNotAbsolute,
    noDots,
    staticPolicy,
  )
import System.Log.FastLogger.Internal (LogStr, fromLogStr)
import System.Logger
  ( Level (Debug, Info, Trace),
    Logger,
    level,
    log,
    msg,
  )
import System.Logger.Message (msg)
import Types
import Utility (stringToDay)
import Web.Scotty.Trans
  ( Parsable (parseParam),
    delete,
    file,
    get,
    json,
    jsonData,
    middleware,
    param,
    post,
    redirect,
    rescue,
    setHeader,
    status,
  )

webApp :: Middleware -> Scotty
webApp requestLogger = do
  middleware requestLogger
  let corsOrigins = ["https://scottishferryapp.com", "https://www.scottishferryapp.com", "http://localhost:3000"]
  middleware $ cors (const $ Just simpleCorsResourcePolicy {corsOrigins = Just (corsOrigins, False)})
  middleware $ gzip def {gzipFiles = GzipCompress}
  middleware $
    addHeaders
      [ ("X-Frame-Options", "DENY"),
        ("X-Content-Type-Options", "nosniff"),
        ("Content-Security-Policy", "script-src 'self' https:, object-src 'none'; base-uri 'none';")
      ]
  middleware $ staticPolicy (noDots <> isNotAbsolute <> addBase "public")
  get "/" $ do
    setHeader "Content-Type" "text/html"
    file "public/index.html"
  get "/api/services" $ do
    services <- getServices
    json services
  get "/api/services/:serviceID" $ do
    serviceID <- param "serviceID"
    departuresDate <- param "departuresDate" `rescue` (\_ -> return Nothing)
    service <- getService serviceID departuresDate
    json service
  post "/api/installations/:installationID" $ do
    installationID <- param "installationID"
    request <- jsonData
    services <- createInstallation installationID request
    json services
  get "/api/installations/:installationID/push-status" $ do
    installationID <- param "installationID"
    pushStatus <- getPushStatus installationID
    maybe (status status404) json pushStatus
  post "/api/installations/:installationID/push-status" $ do
    installationID <- param "installationID"
    request <- jsonData
    lift $ DB.updatePushEnabled installationID (pushStatusEnabled request)
    pushStatus <- getPushStatus installationID
    maybe (status status404) json pushStatus
  get "/api/installations/:installationID/services" $ do
    installationID <- param "installationID"
    services <- getServicesForInstallation installationID
    json services
  post "/api/installations/:installationID/services" $ do
    installationID <- param "installationID"
    (AddServiceRequest serviceID) <- jsonData
    services <- addServiceToInstallation installationID serviceID
    json services
  delete "/api/installations/:installationID/services/:serviceID" $ do
    installationID <- param "installationID"
    serviceID <- param "serviceID"
    services <- deleteServiceForInstallation installationID serviceID
    json services
  get "/api/vessels" $ do
    vessels <- getVessels
    json vessels

instance Parsable UUID where
  parseParam = maybeToEither "Error parsing UUID" . fromText . toStrict

instance Parsable (Maybe Day) where
  parseParam = Right . stringToDay . unpack

maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither errorMessage Nothing = Left errorMessage
maybeToEither _ (Just value) = Right value

loggerSettings :: Logger -> RequestLoggerSettings
loggerSettings logger = case level logger of
  Trace -> debugSettings
  Debug -> debugSettings
  _ -> infoSettings
  where
    debugSettings :: RequestLoggerSettings
    debugSettings = def {destination = Callback (callbackLog Debug)}

    infoSettings :: RequestLoggerSettings
    infoSettings =
      def {outputFormat = Apache FromFallback, destination = Callback (callbackLog Info)}

    callbackLog :: Level -> LogStr -> IO ()
    callbackLog level str =
      System.Logger.log logger level $ msg $ (removeTrailingNewline . fromLogStr) str

    removeTrailingNewline :: BS.ByteString -> BS.ByteString
    removeTrailingNewline =
      BS.reverse . BS.dropWhile (== fromIntegral (ord '\n')) . BS.reverse

type ServiceLocationLookup = M.Map Int [LocationResponse]

type ServiceVesselLookup = M.Map Int [VesselResponse]

type ServiceOrganisationLookup = M.Map Int OrganisationResponse

type LocationScheduledDeparturesLookup = M.Map Int [DepartureResponse]

type LocationNextDepartureLookup = M.Map Int DepartureResponse

type LocationNextRailDepartureLookup = M.Map Int RailDepartureResponse

getPushStatus :: UUID -> Action (Maybe PushStatus)
getPushStatus installationID = do
  installation <- lift $ DB.getInstallationWithID installationID
  return $ PushStatus . installationPushEnabled <$> installation

getService :: Int -> Maybe Day -> Action (Maybe ServiceResponse)
getService serviceID departuresDate = do
  service <- lift $ DB.getService serviceID
  time <- liftIO getCurrentTime
  locationDepartureLookup <- createDeparturesLookup serviceID departuresDate
  nextDepatureLookup <- createNextDepartureLookup serviceID
  nextRailDepartureLookup <- createNextRailDepartureLookup
  locationLookup <- createLocationLookup (Just locationDepartureLookup) (Just nextDepatureLookup) (Just nextRailDepartureLookup)
  vesselLookup <- createServiceVesselLookup
  organisationLookup <- createServiceOrganisationLookup
  return $ serviceToServiceResponse vesselLookup locationLookup organisationLookup 1 time <$> service

getServices :: Action [ServiceResponse]
getServices = do
  services <- lift DB.getServices
  time <- liftIO getCurrentTime
  locationLookup <- createLocationLookup Nothing Nothing Nothing
  vesselLookup <- createServiceVesselLookup
  organisationLookup <- createServiceOrganisationLookup
  forM (zip [1 ..] services) $ \(sortOrder, service) ->
    return $ serviceToServiceResponse vesselLookup locationLookup organisationLookup sortOrder time service

createInstallation ::
  UUID -> CreateInstallationRequest -> Action [ServiceResponse]
createInstallation installationID (CreateInstallationRequest deviceToken deviceType) =
  do
    awsSNSEndpointARN <-
      registerDeviceToken
        installationID
        deviceToken
        deviceType
    time <- liftIO getCurrentTime
    lift $
      DB.createInstallation
        installationID
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
  services <- lift $ DB.getServicesForInstallation installationID
  time <- liftIO getCurrentTime
  locationLookup <- createLocationLookup Nothing Nothing Nothing
  vesselLookup <- createServiceVesselLookup
  organisationLookup <- createServiceOrganisationLookup
  forM (zip [1 ..] services) $ \(sortOrder, service) ->
    return $ serviceToServiceResponse vesselLookup locationLookup organisationLookup sortOrder time service

getVessels :: Action [VesselResponse]
getVessels = do
  time <- liftIO getCurrentTime
  vessels <- filter (vesselFilter time) <$> lift DB.getVessels
  return $ vesselToVesselResponse <$> vessels
  where
    vesselFilter :: UTCTime -> Vessel -> Bool
    vesselFilter currentTime Vessel {vesselLastReceived = vesselLastReceived} =
      vesselTimeFilter currentTime vesselLastReceived

vesselToVesselResponse :: Vessel -> VesselResponse
vesselToVesselResponse Vessel {..} =
  VesselResponse
    { vesselResponseMmsi = vesselMmsi,
      vesselResponseName = vesselName,
      vesselResponseSpeed = vesselSpeed,
      vesselResponseCourse = vesselCourse,
      vesselResponseLatitude = getLatitude vesselCoordinate,
      vesselResponseLongitude = getLongitude vesselCoordinate,
      vesselResponseLastReceived = vesselLastReceived
    }

serviceToServiceResponse ::
  ServiceVesselLookup -> ServiceLocationLookup -> ServiceOrganisationLookup -> Int -> UTCTime -> Service -> ServiceResponse
serviceToServiceResponse vesselLookup locationLookup organisationLookup sortOrder currentTime Service {..} =
  ServiceResponse
    { serviceResponseServiceID = serviceID,
      serviceResponseSortOrder = sortOrder,
      serviceResponseArea = serviceArea,
      serviceResponseRoute = serviceRoute,
      serviceResponseStatus =
        serviceStatusForTime
          currentTime
          serviceUpdated
          serviceStatus,
      serviceResponseLocations =
        fromMaybe [] $
          M.lookup serviceID locationLookup,
      serviceResponseAdditionalInfo = serviceAdditionalInfo,
      serviceResponseDisruptionReason = serviceDisruptionReason,
      serviceResponseLastUpdatedDate = serviceLastUpdatedDate,
      serviceResponseVessels =
        fromMaybe [] $
          M.lookup serviceID vesselLookup,
      serviceResponseOperator = M.lookup serviceID organisationLookup,
      serviceResponseScheduledDeparturesAvailable = Just False,
      serviceResponseUpdated = serviceUpdated
    }
  where
    -- Unknown status if over 30 mins ago
    serviceStatusForTime :: UTCTime -> UTCTime -> ServiceStatus -> ServiceStatus
    serviceStatusForTime currentTime serviceUpdated serviceStatus =
      let diff = diffUTCTime currentTime serviceUpdated
       in if diff > 1800 then Unknown else serviceStatus

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
  logger' <- asks logger
  storedInstallation <- lift $ DB.getInstallationWithID installationID
  currentEndpointARN <-
    if isNothing storedInstallation
      then liftIO $ createPushEndpoint logger' deviceToken deviceType
      else return $ installationEndpointARN . fromJust $ storedInstallation
  endpointAttributesResult <-
    liftIO $
      getAttributesForEndpoint logger' currentEndpointARN
  case endpointAttributesResult of
    EndpointAttributesEndpointNotFound ->
      liftIO $ createPushEndpoint logger' deviceToken deviceType
    AttributeResults awsDeviceToken isEnabled -> do
      when (awsDeviceToken /= deviceToken || not isEnabled)
        $ liftIO
          . void
        $ updateDeviceTokenForEndpoint logger' currentEndpointARN deviceToken
      return currentEndpointARN

createNextDepartureLookup :: Int -> Action LocationNextDepartureLookup
createNextDepartureLookup serviceID = do
  time <- liftIO getCurrentTime
  departuresLookup <- createDeparturesLookup serviceID (Just $ utctDay time)
  return $ M.mapMaybe (findNextDepature time) departuresLookup
  where
    findNextDepature :: UTCTime -> [DepartureResponse] -> Maybe DepartureResponse
    findNextDepature time = find (\d -> departureResponseDeparture d > time) . sortOn departureResponseDeparture

createNextRailDepartureLookup :: Action LocationNextRailDepartureLookup
createNextRailDepartureLookup = do
  time <- liftIO getCurrentTime
  railDepartures <- lift $ DB.getLocationRailDepartures (utctDay time)
  let departuresLookup =
        M.fromListWith (++) $
          [(locationID, [locationRailDepartureToRailDepartureResponse locationRailDeparture]) | locationRailDeparture@(LocationRailDeparture locationID _ _ _ _ _ _ _ _) <- railDepartures]
  return $ M.mapMaybe (findNextDepature time) departuresLookup
  where
    locationRailDepartureToRailDepartureResponse :: LocationRailDeparture -> RailDepartureResponse
    locationRailDepartureToRailDepartureResponse railDeparture =
      RailDepartureResponse
        { railDepartureResponseFrom = locationRailDepartureDepartureName railDeparture,
          railDepartureResponseTo = locationRailDepartureDestinationName railDeparture,
          railDepartureResponseDeparture = convertLocalTimeToUTC $ locationRailDepartureScheduledDepartureTime railDeparture,
          railDepartureResponseDepartureInfo = locationRailDepartureEstimatedDepartureTime railDeparture,
          railDepartureResponsePlatform = locationRailDeparturePlatform railDeparture,
          railDepartureResponseIsCancelled = locationRailDepartureCancelled railDeparture
        }

    findNextDepature :: UTCTime -> [RailDepartureResponse] -> Maybe RailDepartureResponse
    findNextDepature time = find (\d -> railDepartureResponseDeparture d > time) . sortOn railDepartureResponseDeparture

createDeparturesLookup :: Int -> Maybe Day -> Action LocationScheduledDeparturesLookup
createDeparturesLookup serviceID departuresDate = do
  time <- liftIO getCurrentTime
  let date = fromMaybe (utctDay time) departuresDate
  locationDepartures <- lift $ DB.getLocationDepartures serviceID date
  return $
    M.fromListWith (++) $
      [(fromLocationID, [departureResponse locationDeparture]) | locationDeparture@(LocationDeparture fromLocationID _ _ _ _ _ _) <- reverse locationDepartures]
  where
    departureResponse :: LocationDeparture -> DepartureResponse
    departureResponse LocationDeparture {..} =
      DepartureResponse
        { departureResponseDestination =
            LocationResponse
              { locationResponseID = locationDepartureToLocationID,
                locationResponseName = locationDepartureToLocationName,
                locationResponseLatitude = getLatitude locationDepartureToLocationCoordinate,
                locationResponseLongitude = getLatitude locationDepartureToLocationCoordinate,
                locationResponseScheduledDepartures = Nothing,
                locationResponseNextDeparture = Nothing,
                locationResponseNextRailDeparture = Nothing,
                locationResponseWeather = Nothing
              },
          departureResponseDeparture = convertLocalTimeToUTC locationDepartureDepartue,
          departureResponseArrival = convertLocalTimeToUTC locationDepartureArrival,
          departureResponseNotes = locationDepartureNotes
        }

createLocationLookup :: Maybe LocationScheduledDeparturesLookup -> Maybe LocationNextDepartureLookup -> Maybe LocationNextRailDepartureLookup -> Action ServiceLocationLookup
createLocationLookup scheduledDeparturesLookup nextDepatureLookup nextRailDepartureLookup = do
  serviceLocations <- lift DB.getServiceLocations
  locationWeatherLookup <- createLocationWeatherLookup
  return $
    M.fromListWith (++) $
      [ ( serviceID,
          [ LocationResponse
              locationID
              name
              (getLatitude coordinate)
              (getLongitude coordinate)
              (lookupDepartures locationID)
              (lookupNextDepature locationID)
              (lookupNextRailDeparture locationID)
              (M.lookup locationID locationWeatherLookup)
          ]
        )
        | (ServiceLocation serviceID locationID name coordinate) <-
            serviceLocations
      ]
  where
    lookupNextDepature :: Int -> Maybe DepartureResponse
    lookupNextDepature locationID = nextDepatureLookup >>= M.lookup locationID

    lookupNextRailDeparture :: Int -> Maybe RailDepartureResponse
    lookupNextRailDeparture locationID = nextRailDepartureLookup >>= M.lookup locationID

    lookupDepartures :: Int -> Maybe [DepartureResponse]
    lookupDepartures locationID = fromMaybe [] . M.lookup locationID <$> scheduledDeparturesLookup

createServiceOrganisationLookup :: Action ServiceOrganisationLookup
createServiceOrganisationLookup = do
  serviceOrganisations <- lift DB.getServiceOrganisations
  return $
    M.fromList $
      [ (serviceID, OrganisationResponse organisationID name website localNumber internationalNumber email x facebook)
        | (ServiceOrganisation serviceID organisationID name website localNumber internationalNumber email x facebook) <- serviceOrganisations
      ]

createServiceVesselLookup :: Action ServiceVesselLookup
createServiceVesselLookup = do
  serviceVessels <- lift DB.getServiceVessels
  time <- liftIO getCurrentTime
  return $
    M.fromListWith (++) $
      [ (serviceID, [vesselToVesselResponse (Vessel mmsi name speed course coordinate lastReceived updated organisationID)])
        | vessel@(ServiceVessel serviceID mmsi name speed course coordinate lastReceived updated organisationID) <- serviceVessels,
          serviceVesselFilter time vessel
      ]
  where
    serviceVesselFilter :: UTCTime -> ServiceVessel -> Bool
    serviceVesselFilter currentTime ServiceVessel {serviceVesselLastReceived = vesselLastReceived} =
      vesselTimeFilter currentTime vesselLastReceived

createLocationWeatherLookup :: Action (M.Map Int LocationWeatherResponse)
createLocationWeatherLookup = do
  locationWeathers <- lift DB.getLocationWeathers
  return $
    M.fromList $
      [ ( locationID,
          LocationWeatherResponse icon (capitalized description) (kelvinToCelsius temperature) (metersPerSecondToMilesPerHour windSpeed) windDirection (directionToCardinal windDirection)
        )
        | (LocationWeather locationID description icon temperature windSpeed windDirection _ _) <-
            locationWeathers
      ]
  where
    kelvinToCelsius :: Scientific -> Int
    kelvinToCelsius kelvin = round . toRealFloat $ kelvin - 273.15

    metersPerSecondToMilesPerHour :: Scientific -> Int
    metersPerSecondToMilesPerHour mps = round . toRealFloat $ mps * 2.236936284

    directionToCardinal :: Scientific -> String
    directionToCardinal degrees =
      let index = floor ((toRealFloat degrees + 11.25) / 22.5)
          cardinals = ["N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"]
       in cardinals !! (index `mod` 16)

    capitalized :: String -> String
    capitalized (x : xs) = Char.toUpper x : map Char.toLower xs
    capitalized [] = []

getLatitude :: Geometry -> Scientific
getLatitude (GeoPoint _ (Point (Position latitude _ _ _))) = fromFloatDigits latitude
getLatitude _ = error "Expected point"

getLongitude :: Geometry -> Scientific
getLongitude (GeoPoint _ (Point (Position _ longitude _ _))) = fromFloatDigits longitude
getLongitude _ = error "Expected point"

vesselTimeFilter :: UTCTime -> UTCTime -> Bool
vesselTimeFilter currentTime lastReceived =
  let diff = diffUTCTime currentTime lastReceived
   in diff < 1800

convertLocalTimeToUTC :: LocalTime -> UTCTime
convertLocalTimeToUTC = localTimeToUTC (read "UTC")
