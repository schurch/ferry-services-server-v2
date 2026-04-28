{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebServer where

import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO)
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
  )
import Data.Pool (withResource)
import Data.Scientific
  ( Scientific (..),
    fromFloatDigits,
    toRealFloat,
  )
import qualified Data.Set as S
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
import App.Logger
  ( Level (Debug, Info, Trace),
    Logger,
    level,
    logMessage,
  )
import Types
import Types.Api
import Utility (stringToDay)
import qualified Push
import Web.Scotty.Trans
  ( Parsable (parseParam),
    captureParam,
    delete,
    file,
    get,
    json,
    jsonData,
    middleware,
    post,
    queryParamMaybe,
    redirect,
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
    serviceID <- captureParam "serviceID"
    departuresDateParam <- queryParamMaybe "departuresDate"
    let departuresDate = (departuresDateParam :: Maybe Text) >>= stringToDay . unpack
    service <- getService serviceID departuresDate
    json service
  post "/api/installations/:installationID" $ do
    installationID <- captureParam "installationID"
    request <- jsonData
    services <- createInstallation installationID request
    json services
  get "/api/installations/:installationID/push-status" $ do
    installationID <- captureParam "installationID"
    pushStatus <- getPushStatus installationID
    maybe (status status404) json pushStatus
  post "/api/installations/:installationID/push-status" $ do
    installationID <- captureParam "installationID"
    request <- jsonData
    lift $ DB.updatePushEnabled installationID (pushStatusEnabled request)
    pushStatus <- getPushStatus installationID
    maybe (status status404) json pushStatus
  get "/api/installations/:installationID/services" $ do
    installationID <- captureParam "installationID"
    services <- getServicesForInstallation installationID
    json services
  post "/api/installations/:installationID/services" $ do
    installationID <- captureParam "installationID"
    (AddServiceRequest serviceID) <- jsonData
    services <- addServiceToInstallation installationID serviceID
    json services
  delete "/api/installations/:installationID/services/:serviceID" $ do
    installationID <- captureParam "installationID"
    serviceID <- captureParam "serviceID"
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
      logMessage logger level $ removeTrailingNewline $ fromLogStr str

    removeTrailingNewline :: BS.ByteString -> BS.ByteString
    removeTrailingNewline =
      BS.reverse . BS.dropWhile (== fromIntegral (ord '\n')) . BS.reverse

type ServiceLocationLookup = M.Map Int [LocationResponse]

type ServiceVesselLookup = M.Map Int [VesselResponse]

type ServiceOrganisationLookup = M.Map Int OrganisationResponse

type ServiceScheduledDeparturesLookup = S.Set Int

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
  hasScheduledDepartures <- lift $ DB.getServiceHasScheduledDeparturesV2 serviceID
  nextDepatureLookup <- createNextDepartureLookup serviceID
  nextRailDepartureLookup <- createNextRailDepartureLookup
  locationLookup <- createLocationLookup (Just locationDepartureLookup) (Just nextDepatureLookup) (Just nextRailDepartureLookup)
  vesselLookup <- createServiceVesselLookup
  organisationLookup <- createServiceOrganisationLookup
  let scheduledDeparturesLookup =
        if hasScheduledDepartures
          then S.singleton serviceID
          else S.empty
  return $ serviceToServiceResponse scheduledDeparturesLookup vesselLookup locationLookup organisationLookup 1 time <$> service

getServices :: Action [ServiceResponse]
getServices = do
  services <- lift DB.getServices
  time <- liftIO getCurrentTime
  scheduledDeparturesLookup <- createServiceScheduledDeparturesLookup
  locationLookup <- createLocationLookup Nothing Nothing Nothing
  vesselLookup <- createServiceVesselLookup
  organisationLookup <- createServiceOrganisationLookup
  forM (zip [1 ..] services) $ \(sortOrder, service) ->
    return $ serviceToServiceResponse scheduledDeparturesLookup vesselLookup locationLookup organisationLookup sortOrder time service

createInstallation ::
  UUID -> CreateInstallationRequest -> Action [ServiceResponse]
createInstallation installationID (CreateInstallationRequest deviceToken deviceType) =
  do
    awsSNSEndpointARN <-
      lift $
      Push.registerDeviceToken
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
  scheduledDeparturesLookup <- createServiceScheduledDeparturesLookup
  locationLookup <- createLocationLookup Nothing Nothing Nothing
  vesselLookup <- createServiceVesselLookup
  organisationLookup <- createServiceOrganisationLookup
  forM (zip [1 ..] services) $ \(sortOrder, service) ->
    return $ serviceToServiceResponse scheduledDeparturesLookup vesselLookup locationLookup organisationLookup sortOrder time service

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
  ServiceScheduledDeparturesLookup -> ServiceVesselLookup -> ServiceLocationLookup -> ServiceOrganisationLookup -> Int -> UTCTime -> Service -> ServiceResponse
serviceToServiceResponse scheduledDeparturesLookup vesselLookup locationLookup organisationLookup sortOrder currentTime Service {..} =
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
      serviceResponseScheduledDeparturesAvailable = Just $ S.member serviceID scheduledDeparturesLookup,
      serviceResponseUpdated = serviceUpdated
    }
  where
    -- Unknown status if over 30 mins ago
    serviceStatusForTime :: UTCTime -> UTCTime -> ServiceStatus -> ServiceStatus
    serviceStatusForTime currentTime serviceUpdated serviceStatus =
      let diff = diffUTCTime currentTime serviceUpdated
       in if diff > 1800 then Unknown else serviceStatus

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
  locationDepartures <- lift $ DB.getLocationDeparturesV2 serviceID date
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
                locationResponseLongitude = getLongitude locationDepartureToLocationCoordinate,
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

createServiceScheduledDeparturesLookup :: Action ServiceScheduledDeparturesLookup
createServiceScheduledDeparturesLookup =
  S.fromList <$> lift DB.getServicesWithScheduledDeparturesV2

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
