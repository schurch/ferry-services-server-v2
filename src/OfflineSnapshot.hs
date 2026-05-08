{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module OfflineSnapshot
  ( OfflineSnapshotMetadata (..),
    defaultSnapshotPath,
    defaultSnapshotMetadataPath,
    generateOfflineSnapshot,
    writeOfflineSnapshot,
    generateAndWriteOfflineSnapshot,
  )
where

import App.Env (Application)
import App.Logger (logInfoM)
import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
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
import Data.Scientific (Scientific, toRealFloat)
import Data.Time
  ( LocalTime,
    UTCTime (UTCTime),
    addDays,
    defaultTimeLocale,
    formatTime,
    getCurrentTime,
    utctDay,
  )
import Data.Time.Calendar (Day)
import qualified Database as DB
import Database.SQLite.Simple
  ( Connection,
    close,
    executeMany,
    execute_,
    open,
    withTransaction,
  )
import Database.SQLite.Simple.QQ (sql)
import GHC.Generics (Generic)
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    removeFile,
    renameFile,
  )
import System.FilePath (takeDirectory)
import Types
  ( Coordinate (..),
    Location (..),
    LocationDeparture (..),
    Service (..),
    ServiceLocation (..),
    ServiceOrganisation (..),
    jsonOptions,
  )
import Utility (convertScottishLocalTimeToUTC)

defaultSnapshotPath :: FilePath
defaultSnapshotPath = "offline/snapshot.sqlite3"

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
    offlineSnapshotServiceLocations :: [OfflineServiceLocation],
    offlineSnapshotDepartures :: [OfflineDeparture]
  }
  deriving (Generic, Show)

instance ToJSON OfflineSnapshot where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy OfflineSnapshot)

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
    offlineServiceArea :: String,
    offlineServiceRoute :: String,
    offlineServiceOrganisationId :: Int,
    offlineServiceScheduledDeparturesAvailable :: Bool
  }
  deriving (Generic, Show)

instance ToJSON OfflineService where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy OfflineService)

data OfflineLocation = OfflineLocation
  { offlineLocationId :: Int,
    offlineLocationName :: String,
    offlineLocationLatitude :: Scientific,
    offlineLocationLongitude :: Scientific
  }
  deriving (Generic, Show)

instance ToJSON OfflineLocation where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy OfflineLocation)

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

data OfflineServiceLocation = OfflineServiceLocation
  { offlineServiceLocationServiceId :: Int,
    offlineServiceLocationLocationId :: Int,
    offlineServiceLocationDisplayOrder :: Int
  }
  deriving (Generic, Show)

instance ToJSON OfflineServiceLocation where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy OfflineServiceLocation)

data OfflineDeparture = OfflineDeparture
  { offlineDepartureServiceId :: Int,
    offlineDepartureServiceDate :: Day,
    offlineDepartureFromLocationId :: Int,
    offlineDepartureToLocationId :: Int,
    offlineDepartureDepartureTimeUtc :: UTCTime,
    offlineDepartureArrivalTimeUtc :: UTCTime,
    offlineDepartureNotes :: Maybe String
  }
  deriving (Generic, Show)

instance ToJSON OfflineDeparture where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy OfflineDeparture)

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
    "Generating offline SQLite snapshot for "
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
            offlineSnapshotServices = offlineServices servicesWithDepartures services,
            offlineSnapshotLocations = offlineLocations locations,
            offlineSnapshotOrganisations = offlineOrganisations services serviceOrganisations,
            offlineSnapshotServiceLocations = offlineServiceLocations services serviceLocations,
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
  let dataVersion = offlineSnapshotDataVersion snapshot
      etag = quoteETag dataVersion
      metadata =
        OfflineSnapshotMetadata
          { offlineSnapshotMetadataDataVersion = dataVersion,
            offlineSnapshotMetadataEtag = etag,
            offlineSnapshotMetadataGeneratedAt = offlineSnapshotGeneratedAt snapshot,
            offlineSnapshotMetadataValidFrom = offlineSnapshotValidFrom snapshot,
            offlineSnapshotMetadataValidTo = offlineSnapshotValidTo snapshot
          }
  existingMetadata <- readExistingMetadata metadataPath
  snapshotExists <- doesFileExist snapshotPath
  case existingMetadata of
    Just current | snapshotExists && offlineSnapshotMetadataDataVersion current == dataVersion ->
      pure current
    _ -> do
      writeSnapshotDatabase snapshotPath snapshot
      atomicWrite metadataPath (encode metadata)
      pure metadata

writeSnapshotDatabase :: FilePath -> OfflineSnapshot -> IO ()
writeSnapshotDatabase snapshotPath snapshot = do
  let tempPath = snapshotPath <> ".tmp"
  removeIfExists tempPath
  removeIfExists (tempPath <> "-wal")
  removeIfExists (tempPath <> "-shm")
  connection <- open tempPath
  execute_ connection "PRAGMA foreign_keys = ON"
  execute_ connection "PRAGMA journal_mode = DELETE"
  execute_ connection "PRAGMA synchronous = OFF"
  createSnapshotSchema connection
  insertSnapshotRows connection snapshot
  close connection
  renameFile tempPath snapshotPath

createSnapshotSchema :: Connection -> IO ()
createSnapshotSchema connection = do
  execute_
    connection
    [sql|
      CREATE TABLE metadata (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL
      )
    |]
  execute_
    connection
    [sql|
      CREATE TABLE organisations (
        organisation_id INTEGER PRIMARY KEY,
        name TEXT NOT NULL,
        website TEXT NULL,
        local_number TEXT NULL,
        international_number TEXT NULL,
        email TEXT NULL,
        x TEXT NULL,
        facebook TEXT NULL
      )
    |]
  execute_
    connection
    [sql|
      CREATE TABLE services (
        service_id INTEGER PRIMARY KEY,
        area TEXT NOT NULL,
        route TEXT NOT NULL,
        organisation_id INTEGER NOT NULL REFERENCES organisations (organisation_id),
        scheduled_departures_available INTEGER NOT NULL
      )
    |]
  execute_
    connection
    [sql|
      CREATE TABLE locations (
        location_id INTEGER PRIMARY KEY,
        name TEXT NOT NULL,
        latitude REAL NOT NULL,
        longitude REAL NOT NULL
      )
    |]
  execute_
    connection
    [sql|
      CREATE TABLE service_locations (
        service_id INTEGER NOT NULL REFERENCES services (service_id),
        location_id INTEGER NOT NULL REFERENCES locations (location_id),
        display_order INTEGER NOT NULL,
        PRIMARY KEY (service_id, location_id)
      )
    |]
  execute_
    connection
    [sql|
      CREATE TABLE departures (
        service_id INTEGER NOT NULL REFERENCES services (service_id),
        service_date TEXT NOT NULL,
        from_location_id INTEGER NOT NULL REFERENCES locations (location_id),
        to_location_id INTEGER NOT NULL REFERENCES locations (location_id),
        departure_time_utc TEXT NOT NULL,
        arrival_time_utc TEXT NOT NULL,
        notes TEXT NULL
      )
    |]
  execute_
    connection
    [sql|
      CREATE INDEX departures_service_date_idx
      ON departures (service_id, service_date, departure_time_utc)
    |]
  execute_
    connection
    [sql|
      CREATE VIEW client_services AS
      SELECT
        s.service_id,
        s.area,
        s.route,
        s.organisation_id,
        o.name AS organisation_name,
        s.scheduled_departures_available
      FROM services s
      JOIN organisations o ON o.organisation_id = s.organisation_id
    |]
  execute_
    connection
    [sql|
      CREATE VIEW client_service_locations AS
      SELECT
        sl.service_id,
        sl.location_id,
        l.name,
        l.latitude,
        l.longitude,
        sl.display_order
      FROM service_locations sl
      JOIN locations l ON l.location_id = sl.location_id
    |]
  execute_
    connection
    [sql|
      CREATE VIEW client_departures AS
      SELECT
        d.service_id,
        d.service_date,
        d.from_location_id,
        from_location.name AS from_location_name,
        d.to_location_id,
        to_location.name AS to_location_name,
        d.departure_time_utc,
        d.arrival_time_utc,
        d.notes
      FROM departures d
      JOIN locations from_location ON from_location.location_id = d.from_location_id
      JOIN locations to_location ON to_location.location_id = d.to_location_id
    |]

insertSnapshotRows :: Connection -> OfflineSnapshot -> IO ()
insertSnapshotRows connection snapshot =
  withTransaction connection $ do
    executeMany
      connection
      "INSERT INTO metadata (key, value) VALUES (?, ?)"
      (metadataRows snapshot)
    executeMany
      connection
      "INSERT INTO organisations (organisation_id, name, website, local_number, international_number, email, x, facebook) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      (organisationRow <$> offlineSnapshotOrganisations snapshot)
    executeMany
      connection
      "INSERT INTO services (service_id, area, route, organisation_id, scheduled_departures_available) VALUES (?, ?, ?, ?, ?)"
      (serviceRow <$> offlineSnapshotServices snapshot)
    executeMany
      connection
      "INSERT INTO locations (location_id, name, latitude, longitude) VALUES (?, ?, ?, ?)"
      (locationRow <$> offlineSnapshotLocations snapshot)
    executeMany
      connection
      "INSERT INTO service_locations (service_id, location_id, display_order) VALUES (?, ?, ?)"
      (serviceLocationRow <$> offlineSnapshotServiceLocations snapshot)
    executeMany
      connection
      "INSERT INTO departures (service_id, service_date, from_location_id, to_location_id, departure_time_utc, arrival_time_utc, notes) VALUES (?, ?, ?, ?, ?, ?, ?)"
      (departureRow <$> offlineSnapshotDepartures snapshot)

metadataRows :: OfflineSnapshot -> [(String, String)]
metadataRows snapshot =
  [ ("schema_version", show (offlineSnapshotSchemaVersion snapshot)),
    ("data_version", offlineSnapshotDataVersion snapshot),
    ("generated_at_utc", utcText (offlineSnapshotGeneratedAt snapshot)),
    ("valid_from", show (offlineSnapshotValidFrom snapshot)),
    ("valid_to", show (offlineSnapshotValidTo snapshot))
  ]

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
            fmap (offlineDeparture serviceId day) <$> DB.getLocationDeparturesV2 serviceId day
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

offlineServices :: [Int] -> [Service] -> [OfflineService]
offlineServices servicesWithDepartures services =
  [ OfflineService
      { offlineServiceId = serviceID service,
        offlineServiceArea = serviceArea service,
        offlineServiceRoute = serviceRoute service,
        offlineServiceOrganisationId = serviceOrganisationID service,
        offlineServiceScheduledDeparturesAvailable = serviceID service `elem` servicesWithDepartures
      }
    | service <- services
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

offlineServiceLocations :: [Service] -> [ServiceLocation] -> [OfflineServiceLocation]
offlineServiceLocations services serviceLocations =
  concatMap orderedServiceLocations $
    M.toAscList serviceLocationLookup
  where
    visibleServiceIds = serviceID <$> services

    serviceLocationLookup =
      M.fromListWith
        (++)
        [ (serviceLocationServiceID serviceLocation, [serviceLocationLocationID serviceLocation])
          | serviceLocation <- serviceLocations
          , serviceLocationServiceID serviceLocation `elem` visibleServiceIds
        ]

    orderedServiceLocations (serviceId, locationIds) =
      [ OfflineServiceLocation
          { offlineServiceLocationServiceId = serviceId,
            offlineServiceLocationLocationId = locationId,
            offlineServiceLocationDisplayOrder = displayOrder
          }
        | (displayOrder, locationId) <- zip [0 ..] (sortOn id locationIds)
      ]

offlineDeparture :: Int -> Day -> LocationDeparture -> OfflineDeparture
offlineDeparture serviceId serviceDate LocationDeparture {..} =
  OfflineDeparture
    { offlineDepartureServiceId = serviceId,
      offlineDepartureServiceDate = serviceDate,
      offlineDepartureFromLocationId = locationDepartureFromLocationID,
      offlineDepartureToLocationId = locationDepartureToLocationID,
      offlineDepartureDepartureTimeUtc = convertLocalTimeToUTC locationDepartureDepartue,
      offlineDepartureArrivalTimeUtc = convertLocalTimeToUTC locationDepartureArrival,
      offlineDepartureNotes = locationDepartureNotes
    }

organisationRow :: OfflineOrganisation -> (Int, String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String)
organisationRow OfflineOrganisation {..} =
  ( offlineOrganisationId,
    offlineOrganisationName,
    offlineOrganisationWebsite,
    offlineOrganisationLocalNumber,
    offlineOrganisationInternationalNumber,
    offlineOrganisationEmail,
    offlineOrganisationX,
    offlineOrganisationFacebook
  )

serviceRow :: OfflineService -> (Int, String, String, Int, Bool)
serviceRow OfflineService {..} =
  ( offlineServiceId,
    offlineServiceArea,
    offlineServiceRoute,
    offlineServiceOrganisationId,
    offlineServiceScheduledDeparturesAvailable
  )

locationRow :: OfflineLocation -> (Int, String, Double, Double)
locationRow OfflineLocation {..} =
  ( offlineLocationId,
    offlineLocationName,
    scientificToDouble offlineLocationLatitude,
    scientificToDouble offlineLocationLongitude
  )

serviceLocationRow :: OfflineServiceLocation -> (Int, Int, Int)
serviceLocationRow OfflineServiceLocation {..} =
  ( offlineServiceLocationServiceId,
    offlineServiceLocationLocationId,
    offlineServiceLocationDisplayOrder
  )

departureRow :: OfflineDeparture -> (Int, String, Int, Int, String, String, Maybe String)
departureRow OfflineDeparture {..} =
  ( offlineDepartureServiceId,
    show offlineDepartureServiceDate,
    offlineDepartureFromLocationId,
    offlineDepartureToLocationId,
    utcText offlineDepartureDepartureTimeUtc,
    utcText offlineDepartureArrivalTimeUtc,
    offlineDepartureNotes
  )

convertLocalTimeToUTC :: LocalTime -> UTCTime
convertLocalTimeToUTC = convertScottishLocalTimeToUTC

getLatitude :: Coordinate -> Scientific
getLatitude = coordinateLatitude

getLongitude :: Coordinate -> Scientific
getLongitude = coordinateLongitude

scientificToDouble :: Scientific -> Double
scientificToDouble = toRealFloat

utcText :: UTCTime -> String
utcText = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

readExistingMetadata :: FilePath -> IO (Maybe OfflineSnapshotMetadata)
readExistingMetadata path = do
  exists <- doesFileExist path
  if exists then decode <$> BL.readFile path else pure Nothing

atomicWrite :: FilePath -> BL.ByteString -> IO ()
atomicWrite path body = do
  let tempPath = path <> ".tmp"
  BL.writeFile tempPath body
  renameFile tempPath path

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists $
    removeFile path

snapshotHash :: BL.ByteString -> String
snapshotHash body = "sha256-" <> show (Crypto.hashlazy body :: Crypto.Digest Crypto.SHA256)

quoteETag :: String -> String
quoteETag value = "\"" <> value <> "\""
