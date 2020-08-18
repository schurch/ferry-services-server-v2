{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( startServer
  , fetchStatuses
  )
where

import           Data.Text.Lazy                 ( pack )
import           Data.Maybe                     ( listToMaybe
                                                , fromMaybe
                                                )
import           Web.Scotty
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
import           Control.Monad.IO.Class         ( liftIO )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.SqlQQ
import           Data.ByteString                ( ByteString )
import           Data.Char                      ( toLower )
import           Control.Monad                  ( void )

import qualified Data.ByteString.Lazy.Char8    as C

connectionString :: ByteString
connectionString =
  "postgres://stefanchurch@localhost:5432/ferry-services?sslmode=disable"

startServer :: IO ()
startServer = scotty 3000 $ do
  get "/services" $ do
    services <- liftIO getServices
    Web.Scotty.json services
  get "/services/:serviceID" $ do
    serviceID <- param "serviceID"
    service   <- liftIO $ getServiceDetails serviceID
    Web.Scotty.json service
 where
  getServiceDetails :: Int -> IO (Maybe ServiceDetails)
  getServiceDetails serviceID = do
    dbConnection <- connectPostgreSQL connectionString
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
    dbConnection <- connectPostgreSQL connectionString
    results      <- query_
      dbConnection
      [sql| 
        SELECT service_id, sort_order, area, route, status, additional_info, disruption_reason, last_updated_date, updated 
        FROM services 
      |]
    return $ serviceDetailsToService <$> results

  serviceDetailsToService :: ServiceDetails -> Service
  serviceDetailsToService ServiceDetails {..} = Service
    { serviceServiceID = serviceDetailsServiceID
    , serviceUpdated   = serviceDetailsUpdated
    , serviceSortOrder = serviceDetailsSortOrder
    , serviceArea      = serviceDetailsArea
    , serviceRoute     = serviceDetailsRoute
    , serviceStatus    = serviceDetailsStatus
    }

fetchStatuses :: IO ()
fetchStatuses = do
  serviceDetails <- fetchServiceDetails
  saveServiceDetails serviceDetails
 where
  fetchServiceDetails :: IO [ServiceDetails]
  fetchServiceDetails = do
    responseBody <-
      simpleHTTP (getRequest "http://status.calmac.info/?ajax=json")
        >>= getResponseBody
    let result = eitherDecode (C.pack responseBody)
    case result of
      Left  decodingError         -> error decodingError
      Right serviceDetailsResults -> do
        time <- getCurrentTime
        return
          $   ajaxResultToServiceDetails time
          <$> zip [1 ..] serviceDetailsResults

  ajaxResultToServiceDetails
    :: UTCTime -> (Int, AjaxServiceDetails) -> ServiceDetails
  ajaxResultToServiceDetails time (sortOrder, AjaxServiceDetails {..}) =
    ServiceDetails
      { serviceDetailsServiceID        = read ajaxServiceDetailsCode
      , serviceDetailsUpdated          = time
      , serviceDetailsSortOrder        = sortOrder
      , serviceDetailsArea             = ajaxServiceDetailsDestName
      , serviceDetailsRoute            = ajaxServiceDetailsRouteName
      , serviceDetailsStatus           = imageToStatus ajaxServiceDetailsImage
      , serviceDetailsAdditionalInfo   = Just
                                         $ ajaxServiceDetailsWebDetail
                                         <> fromMaybe "" ajaxServiceDetailsInfoMsg
      , serviceDetailsDisruptionReason = reasonToMaybe ajaxServiceDetailsReason
      , serviceDetailsLastUpdatedDate  = Just
        $ stringToUTCTime ajaxServiceDetailsUpdated
      }
   where
    reasonToMaybe :: String -> Maybe String
    reasonToMaybe "NONE" = Nothing
    reasonToMaybe reason = Just reason

    imageToStatus :: String -> ServiceStatus
    imageToStatus image | image == "normal"    = Normal
                        | image == "beware"    = Disrupted
                        | image == "affected"  = Disrupted
                        | image == "cancelled" = Cancelled
                        | otherwise            = error "Unknown image status"

    stringToUTCTime :: String -> UTCTime
    stringToUTCTime time =
      posixSecondsToUTCTime $ fromInteger (read time) / 1000

  saveServiceDetails :: [ServiceDetails] -> IO ()
  saveServiceDetails serviceDetails = do
    dbConnection <- connectPostgreSQL connectionString
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
      serviceDetails

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

data Service = Service {
      serviceServiceID :: Int
    , serviceSortOrder :: Int
    , serviceArea :: String
    , serviceRoute :: String
    , serviceStatus :: ServiceStatus
    , serviceUpdated :: UTCTime
} deriving (Generic, Show)

instance ToJSON Service where
  toJSON = genericToJSON $ jsonOptions 7

data ServiceDetails = ServiceDetails {
      serviceDetailsServiceID :: Int
    , serviceDetailsSortOrder :: Int
    , serviceDetailsArea :: String
    , serviceDetailsRoute :: String
    , serviceDetailsStatus :: ServiceStatus
    , serviceDetailsAdditionalInfo :: Maybe String
    , serviceDetailsDisruptionReason :: Maybe String
    , serviceDetailsLastUpdatedDate :: Maybe UTCTime
    , serviceDetailsUpdated :: UTCTime
} deriving (Generic, Show, ToRow, FromRow)

instance ToJSON ServiceDetails where
  toJSON = genericToJSON $ jsonOptions 14

jsonOptions :: Int -> Data.Aeson.Options
jsonOptions prefixLength =
  defaultOptions { fieldLabelModifier = camelTo2 '_' . drop prefixLength }

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
