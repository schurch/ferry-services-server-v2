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

createInstallation :: CreateInstallationRequest -> IO [Service]
createInstallation request = return []

addServiceToInstallation :: UUID -> Int -> IO [Service]
addServiceToInstallation installationID serviceID = undefined

deleteServiceForInstallation :: UUID -> Int -> IO [Service]
deleteServiceForInstallation installationID serviceID = undefined

getServicesForInstallation :: UUID -> IO [Service]
getServicesForInstallation installationID = undefined

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

data DeviceType = IOS | Android deriving (Generic, Show, ToField, FromField, ToJSON, FromJSON)

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
  parseJSON = genericParseJSON $ jsonOptions 19

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
