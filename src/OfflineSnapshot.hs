{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OfflineSnapshot
  ( OfflineSnapshot (..),
    OfflineSnapshotMetadata (..),
    defaultSnapshotPath,
    defaultSnapshotMetadataPath,
    generateOfflineSnapshot,
    writeOfflineSnapshot,
    generateAndWriteOfflineSnapshot,
  )
where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import App.Env (Application)
import App.Logger (logInfoM)
import qualified Crypto.Hash as Crypto
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    decode,
    encode,
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.ByteString.Lazy as BL
import Data.List (nubBy, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Time
  ( LocalTime (LocalTime),
    TimeOfDay (TimeOfDay),
    UTCTime (UTCTime),
    addDays,
    addLocalTime,
    defaultTimeLocale,
    formatTime,
    getCurrentTime,
    localDay,
    localTimeToUTC,
    utctDay,
  )
import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength, toGregorian)
import qualified Database as DB
import Database.Postgis
  ( Geometry (GeoPoint),
    Point (Point),
    Position (Position),
  )
import GHC.Generics (Generic)
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    renameFile,
  )
import System.FilePath (takeDirectory)
import System.IO (IOMode (WriteMode), withFile)
import Types
  ( Location (..),
    LocationDeparture (..),
    Service (..),
    ServiceLocation (..),
    ServiceOrganisation (..),
    jsonOptions,
  )

defaultSnapshotPath :: FilePath
defaultSnapshotPath = "offline/snapshot.json"

defaultSnapshotMetadataPath :: FilePath
defaultSnapshotMetadataPath = "offline/snapshot.meta.json"

data OfflineSnapshot = OfflineSnapshot
  { offlineSnapshotSchemaVersion :: Int,
    offlineSnapshotDataVersion :: String,
    offlineSnapshotGeneratedAt :: UTCTime,
    offlineSnapshotValidFrom :: Day,
    offlineSnapshotValidTo :: Day,
    offlineSnapshotServices :: [OfflineService],
    offlineSnapshotLocations :: [OfflineLocation],
    offlineSnapshotOrganisations :: [OfflineOrganisation],
    offlineSnapshotDepartures :: [OfflineDeparture]
  }
  deriving (Generic, Show)

instance ToJSON OfflineSnapshot where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy OfflineSnapshot)

instance FromJSON OfflineSnapshot where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy OfflineSnapshot)

data OfflineSnapshotMetadata = OfflineSnapshotMetadata
  { offlineSnapshotMetadataDataVersion :: String,
    offlineSnapshotMetadataEtag :: String,
    offlineSnapshotMetadataGeneratedAt :: UTCTime,
    offlineSnapshotMetadataValidFrom :: Day,
    offlineSnapshotMetadataValidTo :: Day
  }
  deriving (Generic, Show)

instance ToJSON OfflineSnapshotMetadata where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy OfflineSnapshotMetadata)

instance FromJSON OfflineSnapshotMetadata where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy OfflineSnapshotMetadata)

data OfflineService = OfflineService
  { offlineServiceId :: Int,
    offlineServiceSortOrder :: Int,
    offlineServiceArea :: String,
    offlineServiceRoute :: String,
    offlineServiceOrganisationId :: Int,
    offlineServiceLocationIds :: [Int],
    offlineServiceScheduledDeparturesAvailable :: Bool
  }
  deriving (Generic, Show)

instance ToJSON OfflineService where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy OfflineService)

instance FromJSON OfflineService where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy OfflineService)

data OfflineLocation = OfflineLocation
  { offlineLocationId :: Int,
    offlineLocationName :: String,
    offlineLocationLatitude :: Scientific,
    offlineLocationLongitude :: Scientific
  }
  deriving (Generic, Show)

instance ToJSON OfflineLocation where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy OfflineLocation)

instance FromJSON OfflineLocation where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy OfflineLocation)

data OfflineOrganisation = OfflineOrganisation
  { offlineOrganisationId :: Int,
    offlineOrganisationName :: String,
    offlineOrganisationWebsite :: Maybe String,
    offlineOrganisationLocalNumber :: Maybe String,
    offlineOrganisationInternationalNumber :: Maybe String,
    offlineOrganisationEmail :: Maybe String,
    offlineOrganisationX :: Maybe String,
    offlineOrganisationFacebook :: Maybe String
  }
  deriving (Generic, Show)

instance ToJSON OfflineOrganisation where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy OfflineOrganisation)

instance FromJSON OfflineOrganisation where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy OfflineOrganisation)

data OfflineDeparture = OfflineDeparture
  { offlineDepartureServiceId :: Int,
    offlineDepartureFromLocationId :: Int,
    offlineDepartureToLocationId :: Int,
    offlineDepartureDepartureLocal :: LocalTime,
    offlineDepartureArrivalLocal :: LocalTime,
    offlineDepartureNotes :: Maybe String
  }
  deriving (Generic, Show)

instance ToJSON OfflineDeparture where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy OfflineDeparture)

instance FromJSON OfflineDeparture where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy OfflineDeparture)

generateAndWriteOfflineSnapshot :: Application OfflineSnapshotMetadata
generateAndWriteOfflineSnapshot = do
  snapshot <- generateOfflineSnapshot
  previousMetadata <- liftIO $ readExistingMetadata defaultSnapshotMetadataPath
  metadata <- liftIO $ writeOfflineSnapshot defaultSnapshotPath defaultSnapshotMetadataPath snapshot
  case previousMetadata of
    Just previous | offlineSnapshotMetadataDataVersion previous == offlineSnapshotDataVersion snapshot ->
      logInfoM $
        "Offline snapshot artifact unchanged: "
          <> offlineSnapshotMetadataDataVersion metadata
    _ ->
      logInfoM $
        "Offline snapshot artifact updated: "
          <> offlineSnapshotMetadataDataVersion metadata
  pure metadata

generateOfflineSnapshot :: Application OfflineSnapshot
generateOfflineSnapshot = do
  now <- liftIO getCurrentTime
  let validFrom = utctDay now
      validTo = addDays 59 validFrom
  logInfoM $
    "Generating offline snapshot for "
      <> show validFrom
      <> " to "
      <> show validTo
      <> " ..."
  services <- DB.getServices
  logInfoM $ "Offline snapshot visible services: " <> show (length services)
  locations <- DB.getLocations
  logInfoM $ "Offline snapshot locations: " <> show (length locations)
  serviceLocations <- DB.getServiceLocations
  logInfoM $ "Offline snapshot service-location links: " <> show (length serviceLocations)
  serviceOrganisations <- DB.getServiceOrganisations
  logInfoM $ "Offline snapshot service organisations: " <> show (length serviceOrganisations)
  servicesWithDepartures <- DB.getServicesWithScheduledDeparturesV2
  logInfoM $ "Offline snapshot services with scheduled departures: " <> show (length servicesWithDepartures)
  departures <- createDepartures servicesWithDepartures services validFrom validTo
  logInfoM $ "Offline snapshot generated departures: " <> show (length departures)
  let bodyWithoutVersion =
        OfflineSnapshot
          { offlineSnapshotSchemaVersion = 1,
            offlineSnapshotDataVersion = "",
            offlineSnapshotGeneratedAt = now,
            offlineSnapshotValidFrom = validFrom,
            offlineSnapshotValidTo = validTo,
            offlineSnapshotServices = offlineServices servicesWithDepartures serviceLocations services,
            offlineSnapshotLocations = offlineLocations locations,
            offlineSnapshotOrganisations = offlineOrganisations services serviceOrganisations,
            offlineSnapshotDepartures = departures
          }
      dataVersion = dataVersionFor bodyWithoutVersion
  pure bodyWithoutVersion {offlineSnapshotDataVersion = dataVersion}
  where
    dataVersionFor snapshot =
      snapshotHash $
        encode
          snapshot
            { offlineSnapshotDataVersion = "",
              offlineSnapshotGeneratedAt = UTCTime (offlineSnapshotValidFrom snapshot) 0
            }

writeOfflineSnapshot :: FilePath -> FilePath -> OfflineSnapshot -> IO OfflineSnapshotMetadata
writeOfflineSnapshot snapshotPath metadataPath snapshot = do
  createDirectoryIfMissing True (takeDirectory snapshotPath)
  createDirectoryIfMissing True (takeDirectory metadataPath)
  let body = encode snapshot
      dataVersion = offlineSnapshotDataVersion snapshot
      etag = quoteETag dataVersion
      metadata =
        OfflineSnapshotMetadata
          { offlineSnapshotMetadataDataVersion = dataVersion,
            offlineSnapshotMetadataEtag = etag,
            offlineSnapshotMetadataGeneratedAt = offlineSnapshotGeneratedAt snapshot,
            offlineSnapshotMetadataValidFrom = offlineSnapshotValidFrom snapshot,
            offlineSnapshotMetadataValidTo = offlineSnapshotValidTo snapshot
          }
  existingSnapshot <- readExisting snapshotPath
  existingMetadata <- readExistingMetadata metadataPath
  case (existingSnapshot, existingMetadata) of
    (Just _, Just current) | offlineSnapshotMetadataDataVersion current == dataVersion ->
      pure current
    _ -> do
      atomicWrite snapshotPath body
      atomicWrite metadataPath (encode metadata)
      pure metadata

createDepartures :: [Int] -> [Service] -> Day -> Day -> Application [OfflineDeparture]
createDepartures servicesWithDepartures services validFrom validTo = do
  logInfoM $
    "Offline snapshot departure generation service count: "
      <> show (length serviceIds)
      <> ", days: "
      <> show (length snapshotDays)
  fmap concat $
    forM (zip [1 ..] serviceIds) $ \(index, serviceId) -> do
      logDepartureServiceProgress index serviceId
      serviceDepartures <-
        fmap concat $
          forM snapshotDays $ \day ->
            fmap (offlineDeparture serviceId) <$> DB.getLocationDeparturesV2 serviceId day
      logInfoM $
        "Offline snapshot departures for service "
          <> show serviceId
          <> ": "
          <> show (length serviceDepartures)
      pure serviceDepartures
  where
    serviceIds =
      filter (`elem` servicesWithDepartures) $
        serviceID <$> services

    snapshotDays =
      takeWhile (<= validTo) $
        iterate (addDays 1) validFrom

    logDepartureServiceProgress index serviceId =
      if shouldLogProgress index
        then
          logInfoM $
            "Offline snapshot departure progress "
              <> show index
              <> "/"
              <> show (length serviceIds)
              <> " service_id="
              <> show serviceId
        else pure ()

    shouldLogProgress index =
      index == 1 || index == length serviceIds || index `mod` 10 == 0

offlineServices :: [Int] -> [ServiceLocation] -> [Service] -> [OfflineService]
offlineServices servicesWithDepartures serviceLocations services =
  [ OfflineService
      { offlineServiceId = serviceID service,
        offlineServiceSortOrder = sortOrder,
        offlineServiceArea = serviceArea service,
        offlineServiceRoute = serviceRoute service,
        offlineServiceOrganisationId = serviceOrganisationID service,
        offlineServiceLocationIds = sortOn id $ M.findWithDefault [] (serviceID service) serviceLocationLookup,
        offlineServiceScheduledDeparturesAvailable = serviceID service `elem` servicesWithDepartures
      }
    | (sortOrder, service) <- zip [1 ..] services
  ]
  where
    serviceLocationLookup =
      M.fromListWith
        (++)
        [ (serviceLocationServiceID serviceLocation, [serviceLocationLocationID serviceLocation])
          | serviceLocation <- serviceLocations
        ]

offlineLocations :: [Location] -> [OfflineLocation]
offlineLocations locations =
  [ OfflineLocation
      { offlineLocationId = locationLocationID location,
        offlineLocationName = locationName location,
        offlineLocationLatitude = getLatitude $ locationCoordinate location,
        offlineLocationLongitude = getLongitude $ locationCoordinate location
      }
    | location <- sortOn locationLocationID locations
  ]

offlineOrganisations :: [Service] -> [ServiceOrganisation] -> [OfflineOrganisation]
offlineOrganisations services serviceOrganisations =
  sortOn offlineOrganisationId $
    nubBy sameOrganisation $
      mapMaybe toOfflineOrganisation serviceOrganisations
  where
    visibleServiceIds = serviceID <$> services

    toOfflineOrganisation organisation
      | serviceOrganisationServiceID organisation `elem` visibleServiceIds =
          Just
            OfflineOrganisation
              { offlineOrganisationId = serviceOrganisationOrganisationID organisation,
                offlineOrganisationName = serviceOrganisationName organisation,
                offlineOrganisationWebsite = serviceOrganisationWebsite organisation,
                offlineOrganisationLocalNumber = serviceOrganisationLocalPhone organisation,
                offlineOrganisationInternationalNumber = serviceOrganisationInternationalPhone organisation,
                offlineOrganisationEmail = serviceOrganisationEmail organisation,
                offlineOrganisationX = serviceOrganisationX organisation,
                offlineOrganisationFacebook = serviceOrganisationFacebook organisation
              }
      | otherwise = Nothing

    sameOrganisation left right =
      offlineOrganisationId left == offlineOrganisationId right

offlineDeparture :: Int -> LocationDeparture -> OfflineDeparture
offlineDeparture serviceId LocationDeparture {..} =
  OfflineDeparture
    { offlineDepartureServiceId = serviceId,
      offlineDepartureFromLocationId = locationDepartureFromLocationID,
      offlineDepartureToLocationId = locationDepartureToLocationID,
      offlineDepartureDepartureLocal = utcClockLocalTimeToEuropeLondon locationDepartureDepartue,
      offlineDepartureArrivalLocal = utcClockLocalTimeToEuropeLondon locationDepartureArrival,
      offlineDepartureNotes = locationDepartureNotes
    }

utcClockLocalTimeToEuropeLondon :: LocalTime -> LocalTime
utcClockLocalTimeToEuropeLondon utcClockTime =
  addLocalTime offsetSeconds utcClockTime
  where
    offsetSeconds =
      if isBritishSummerTime utcClockTime
        then 3600
        else 0

isBritishSummerTime :: LocalTime -> Bool
isBritishSummerTime utcClockTime =
  utcTime >= bstStart && utcTime < bstEnd
  where
    utcTime = localTimeToUTC (read "UTC") utcClockTime
    (year, _, _) = toGregorian (localDay utcClockTime)
    bstStart = localTimeToUTC (read "UTC") $ LocalTime (lastSundayOfMonth year 3) (TimeOfDay 1 0 0)
    bstEnd = localTimeToUTC (read "UTC") $ LocalTime (lastSundayOfMonth year 10) (TimeOfDay 1 0 0)

lastSundayOfMonth :: Integer -> Int -> Day
lastSundayOfMonth year month =
  last $
    filter isSunday $
      [fromGregorian year month day | day <- [1 .. monthLength]]
  where
    monthLength =
      gregorianMonthLength year month

    isSunday day =
      formatTime defaultTimeLocale "%u" day == "7"

getLatitude :: Geometry -> Scientific
getLatitude (GeoPoint _ (Point (Position latitude _ _ _))) = fromFloatDigits latitude
getLatitude _ = error "Expected point"

getLongitude :: Geometry -> Scientific
getLongitude (GeoPoint _ (Point (Position _ longitude _ _))) = fromFloatDigits longitude
getLongitude _ = error "Expected point"

readExisting :: FilePath -> IO (Maybe BL.ByteString)
readExisting path = do
  exists <- doesFileExist path
  if exists then Just <$> BL.readFile path else pure Nothing

readExistingMetadata :: FilePath -> IO (Maybe OfflineSnapshotMetadata)
readExistingMetadata path =
  (>>= decode) <$> readExisting path

atomicWrite :: FilePath -> BL.ByteString -> IO ()
atomicWrite path body = do
  let tempPath = path <> ".tmp"
  withFile tempPath WriteMode $ \handle -> BL.hPut handle body
  renameFile tempPath path

snapshotHash :: BL.ByteString -> String
snapshotHash body = "sha256-" <> show (Crypto.hashlazy body :: Crypto.Digest Crypto.SHA256)

quoteETag :: String -> String
quoteETag value = "\"" <> value <> "\""
