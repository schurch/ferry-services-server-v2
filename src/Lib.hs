{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( getService
  , getServices
  , createInstallation
  , addServiceToInstallation
  , deleteServiceForInstallation
  , getServicesForInstallation
  , fetchStatuses
  , AddServiceRequest(..)
  )
where

import           Data.Text.Lazy                 ( pack )
import           Data.Maybe                     ( listToMaybe
                                                , fromMaybe
                                                )
import           Data.Aeson
import           GHC.Generics
import           Network.HTTP                   ( simpleHTTP
                                                , getRequest
                                                , getResponseBody
                                                )
import           Debug.Trace
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.SqlQQ
import           Data.ByteString                ( ByteString )
import           Data.Char                      ( toLower )
import           Control.Monad                  ( void )
import           Data.String                    ( fromString )
import           System.Environment             ( getEnv )
import           Data.UUID                      ( UUID )

import qualified Data.ByteString.Lazy.Char8    as C

connectionString :: IO ByteString
connectionString = fromString <$> getEnv "DB_CONNECTION"

getService :: Int -> IO (Maybe Service)
getService serviceID = do
  dbConnection <- connectionString >>= connectPostgreSQL
  results      <- query
    dbConnection
    [sql| 
      SELECT service_id, sort_order, area, route, status, additional_info, disruption_reason, last_updated_date, updated 
      FROM services 
      WHERE service_id = ? 
    |]
    (Only serviceID)
  return $ listToMaybe results

getServices :: IO [Service]
getServices = do
  dbConnection <- connectionString >>= connectPostgreSQL
  query_
    dbConnection
    [sql| 
      SELECT service_id, sort_order, area, route, status, additional_info, disruption_reason, last_updated_date, updated 
      FROM services 
    |]

createInstallation :: UUID -> CreateInstallationRequest -> IO [Service]
createInstallation installationID (CreateInstallationRequest deviceToken deviceType)
  = do
    dbConnection      <- connectionString >>= connectPostgreSQL
    awsSNSEndpointARN <- registerDeviceToken installationID
                                             deviceToken
                                             deviceType
    time <- getCurrentTime
    void $ execute
      dbConnection
      [sql|
        INSERT INTO installations (installation_id, device_token, device_type, endpoint_arn, updated) 
          VALUES (?,?,?,?,?)
          ON CONFLICT (installation_id) DO UPDATE 
            SET installation_id = excluded.installation_id, 
                device_token = excluded.device_token, 
                device_type = excluded.device_type, 
                endpoint_arn = excluded.endpoint_arn, 
                updated = excluded.updated
      |]
      (installationID, deviceToken, deviceType, awsSNSEndpointARN, time)
    getServicesForInstallation installationID

addServiceToInstallation :: UUID -> Int -> IO [Service]
addServiceToInstallation installationID serviceID = do
  dbConnection <- connectionString >>= connectPostgreSQL
  void $ execute
    dbConnection
    [sql|
        INSERT INTO installation_services (installation_id, service_id) 
        VALUES (?,?)
    |]
    (installationID, serviceID)
  getServicesForInstallation installationID

deleteServiceForInstallation :: UUID -> Int -> IO [Service]
deleteServiceForInstallation installationID serviceID = do
  dbConnection <- connectionString >>= connectPostgreSQL
  void $ execute
    dbConnection
    [sql|
        DELETE FROM installation_services WHERE installation_id = ? AND service_id = ?
    |]
    (installationID, serviceID)
  getServicesForInstallation installationID

getServicesForInstallation :: UUID -> IO [Service]
getServicesForInstallation installationID = do
  dbConnection <- connectionString >>= connectPostgreSQL
  query
    dbConnection
    [sql| 
        SELECT s.service_id, s.sort_order, s.area, s.route, s.status, s.additional_info, s.disruption_reason, s.last_updated_date, s.updated 
        FROM services s
        JOIN installation_services i ON s.service_id = i.service_id
        WHERE i.installation_id = ?
      |]
    (Only installationID)

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
registerDeviceToken :: UUID -> String -> DeviceType -> IO String
registerDeviceToken installationID token deviceType = do
  dbConnection <- connectionString >>= connectPostgreSQL
  return "12345"

fetchStatuses :: IO ()
fetchStatuses = do
  services <- fetchServices
  saveServices services
 where
  fetchServices :: IO [Service]
  fetchServices = do
    responseBody <-
      simpleHTTP (getRequest "http://status.calmac.info/?ajax=json")
        >>= getResponseBody
    let result = eitherDecode (C.pack responseBody)
    case result of
      Left  decodingError         -> error decodingError
      Right serviceDetailsResults -> do
        time <- getCurrentTime
        return $ ajaxResultToService time <$> zip [1 ..] serviceDetailsResults

  ajaxResultToService :: UTCTime -> (Int, AjaxServiceDetails) -> Service
  ajaxResultToService time (sortOrder, AjaxServiceDetails {..}) = Service
    { serviceID               = read ajaxServiceDetailsCode
    , serviceUpdated          = time
    , serviceSortOrder        = sortOrder
    , serviceArea             = ajaxServiceDetailsDestName
    , serviceRoute            = ajaxServiceDetailsRouteName
    , serviceStatus           = imageToStatus ajaxServiceDetailsImage
    , serviceAdditionalInfo   = Just
                                $  ajaxServiceDetailsWebDetail
                                <> fromMaybe "" ajaxServiceDetailsInfoMsg
    , serviceDisruptionReason = reasonToMaybe ajaxServiceDetailsReason
    , serviceLastUpdatedDate  = Just $ stringToUTCTime ajaxServiceDetailsUpdated
    }
   where
    reasonToMaybe :: String -> Maybe String
    reasonToMaybe reason | reason == "NONE" = Nothing
                         | otherwise        = Just reason

    imageToStatus :: String -> ServiceStatus
    imageToStatus image | image == "normal"    = Normal
                        | image == "beware"    = Disrupted
                        | image == "affected"  = Disrupted
                        | image == "cancelled" = Cancelled
                        | otherwise            = error "Unknown image status"

    stringToUTCTime :: String -> UTCTime
    stringToUTCTime time =
      posixSecondsToUTCTime $ fromInteger (read time) / 1000

  saveServices :: [Service] -> IO ()
  saveServices service = do
    dbConnection <- connectionString >>= connectPostgreSQL
    void $ executeMany
      dbConnection
      [sql| 
        INSERT INTO services (service_id, sort_order, area, route, status, additional_info, disruption_reason, last_updated_date, updated) 
        VALUES (?,?,?,?,?,?,?,?,?)
        ON CONFLICT (service_id) DO UPDATE 
          SET service_id = excluded.service_id, 
              sort_order = excluded.sort_order, 
              area = excluded.area, 
              route = excluded.route, 
              status = excluded.status, 
              additional_info = excluded.additional_info, 
              disruption_reason = excluded.disruption_reason,
              last_updated_date = excluded.last_updated_date,
              updated = excluded.updated
      |]
      service

-- Types
data ServiceStatus = Normal | Disrupted | Cancelled | Unknown deriving (Show, Eq)

instance Enum ServiceStatus where
  toEnum 0     = Normal
  toEnum 1     = Disrupted
  toEnum 2     = Cancelled
  toEnum (-99) = Unknown

  fromEnum Normal    = 0
  fromEnum Disrupted = 1
  fromEnum Cancelled = 2
  fromEnum Unknown   = -99

instance ToJSON ServiceStatus where
  toJSON = toJSON . fromEnum

instance ToField ServiceStatus where
  toField = toField . fromEnum

instance FromField ServiceStatus where
  fromField field byteString = toEnum <$> fromField field byteString


data DeviceType = IOS | Android deriving (Eq, Show, Generic, Bounded, Enum)

instance ToJSON DeviceType

instance FromJSON DeviceType

instance ToField DeviceType where
  toField = toField . fromEnum

instance FromField DeviceType where
  fromField field byteString = toEnum <$> fromField field byteString

-- Database Types
data Service = Service {
    serviceID :: Int
  , serviceSortOrder :: Int
  , serviceArea :: String
  , serviceRoute :: String
  , serviceStatus :: ServiceStatus
  , serviceAdditionalInfo :: Maybe String
  , serviceDisruptionReason :: Maybe String
  , serviceLastUpdatedDate :: Maybe UTCTime
  , serviceUpdated :: UTCTime
} deriving (Generic, Show, ToRow, FromRow)

instance ToJSON Service where
  toJSON = genericToJSON $ jsonOptions 7

data Installation = Installation {
    installationID :: UUID
  , installationDeviceToken :: String
  , installationDeviceType :: DeviceType
  , installationpdatedDate :: UTCTime
} deriving (Generic, Show, ToRow, FromRow)

-- API Types

data CreateInstallationRequest = CreateInstallationRequest {
    createInstallationRequestDeviceToken :: String
  , createInstallationRequestDeviceType :: DeviceType
} deriving (Generic, Show)

instance FromJSON CreateInstallationRequest where
  parseJSON = genericParseJSON $ jsonOptions 25

data AddServiceRequest = AddServiceRequest {
    addServiceRequestServiceID :: Int
} deriving (Generic, Show)

instance FromJSON AddServiceRequest where
  parseJSON = genericParseJSON $ jsonOptions 17

jsonOptions :: Int -> Data.Aeson.Options
jsonOptions prefixLength =
  defaultOptions { fieldLabelModifier = camelTo2 '_' . drop prefixLength }

-- Scraper Types
data AjaxServiceDetails = AjaxServiceDetails {
    ajaxServiceDetailsReason :: String
  , ajaxServiceDetailsImage :: String
  , ajaxServiceDetailsDestName :: String
  , ajaxServiceDetailsCode :: String
  , ajaxServiceDetailsInfoIncluded :: String
  , ajaxServiceDetailsInfoMsg :: Maybe String
  , ajaxServiceDetailsReported :: String
  , ajaxServiceDetailsId :: String
  , ajaxServiceDetailsWebDetail :: String
  , ajaxServiceDetailsUpdated :: String
  , ajaxServiceDetailsRouteName :: String
  , ajaxServiceDetailsStatus :: String
} deriving (Generic, Show)

instance FromJSON AjaxServiceDetails where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = toLowerFirstLetter . drop 18 }

toLowerFirstLetter :: String -> String
toLowerFirstLetter []       = []
toLowerFirstLetter (x : xs) = toLower x : xs
