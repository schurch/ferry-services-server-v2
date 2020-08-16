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
import           Data.Maybe                     ( listToMaybe )
import           Web.Scotty
import           Data.Aeson
import           GHC.Generics
import           Network.HTTP                   ( simpleHTTP
                                                , getRequest
                                                , getResponseBody
                                                )
import           Debug.Trace
import           Text.HTML.TagSoup
import           Data.List.Split                ( splitOn )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad                  ( forM )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.SqlQQ
import           Data.ByteString                ( ByteString )
import           Data.List                      ( intercalate )
import           Data.Time                      ( parseTimeOrError
                                                , defaultTimeLocale
                                                )
import           Control.Exception

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
  services       <- fetchServices
  serviceDetails <- forM services fetchServiceDetailsForService
  saveServiceDetails serviceDetails
 where
  fetchServices :: IO [Service]
  fetchServices = do
    responseBody <-
      simpleHTTP (getRequest "http://status.calmac.info") >>= getResponseBody
    let tags           = parseTags responseBody
    let routeItems = sections (~== ("<li class=route>" :: String)) $ tags
    let itemsWithOrder = zip [1 ..] routeItems
    time <- getCurrentTime
    return $ routeItemToService time <$> itemsWithOrder

  fetchServiceDetailsForService :: Service -> IO ServiceDetails
  fetchServiceDetailsForService service = do
    let serviceIDString = (show $ serviceServiceID service)
    putStrLn $ "Fetching details for serviceID: " <> serviceIDString
    let url = "http://status.calmac.info/?route=" <> serviceIDString
    responseBody <- simpleHTTP (getRequest url) >>= getResponseBody
    let tags = parseTags responseBody
    additionalInfo <-
      try $ evaluate $ additionalInformationFromTags tags :: IO
        (Either SomeException String)
    reason <-
      try $ evaluate $ reasonFromTags tags :: IO (Either SomeException String)
    updatedDate <-
      try $ evaluate $ updatedDateFromTags tags :: IO
        (Either SomeException UTCTime)
    let serviceDetails = (serviceToServiceDetails service)
          { serviceDetailsAdditionalInfo   = rightToMaybe additionalInfo
          , serviceDetailsDisruptionReason = rightToMaybe reason
          , serviceDetailsLastUpdatedDate  = rightToMaybe updatedDate
          }
    return serviceDetails

  rightToMaybe :: Either a b -> Maybe b
  rightToMaybe = either (const Nothing) Just

  saveServiceDetails :: [ServiceDetails] -> IO ()
  saveServiceDetails serviceDetails = do
    dbConnection <- connectPostgreSQL connectionString
    executeMany
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
    return ()

  routeItemToService :: UTCTime -> (Int, [Tag String]) -> Service
  routeItemToService time (sortOrder, routeItem) = Service
    { serviceServiceID = serviceIDFromURL . routeURLFromTags $ routeItem
    , serviceUpdated   = time
    , serviceSortOrder = sortOrder
    , serviceArea      = areaFromTags routeItem
    , serviceRoute     = routeFromTags routeItem
    , serviceStatus    = serviceStatusFromTags routeItem
    }

  serviceToServiceDetails :: Service -> ServiceDetails
  serviceToServiceDetails Service {..} = ServiceDetails
    { serviceDetailsServiceID        = serviceServiceID
    , serviceDetailsUpdated          = serviceUpdated
    , serviceDetailsSortOrder        = serviceSortOrder
    , serviceDetailsArea             = serviceArea
    , serviceDetailsRoute            = serviceRoute
    , serviceDetailsStatus           = serviceStatus
    , serviceDetailsAdditionalInfo   = Nothing
    , serviceDetailsDisruptionReason = Nothing
    , serviceDetailsLastUpdatedDate  = Nothing
    }

  updatedDateFromTags :: [Tag String] -> UTCTime
  updatedDateFromTags tags =
    let lastUpdatedTagText =
            fromTagText $ dropWhile (~/= ("Last Updated" :: String)) tags !! 2
        updatedText =
            drop 1 . replace "\n" "" . replace "\t" "" $ lastUpdatedTagText
    in  parseTimeOrError True
                         defaultTimeLocale
                         "%d %h %Y %H:%M %Z"
                         (updatedText <> " GMT")

  reasonFromTags :: [Tag String] -> String
  reasonFromTags tags =
    drop 2 . fromTagText $ dropWhile (~/= ("Reason" :: String)) tags !! 2

  additionalInformationFromTags :: [Tag String] -> String
  additionalInformationFromTags tags =
    let tagRange = takeWhile (~/= ("<script>" :: String))
          $ dropWhile (~/= ("<div data-role=content>" :: String)) tags
    in  renderTags $ take (length tagRange - 5) tagRange

  routeURLFromTags :: [Tag String] -> String
  routeURLFromTags tags = fromAttrib "href" $ tags !! 1

  areaFromTags :: [Tag String] -> String
  areaFromTags tags = fromTagText $ tags !! 7

  routeFromTags :: [Tag String] -> String
  routeFromTags tags = fromTagText $ tags !! 11

  serviceIDFromURL :: String -> Int
  serviceIDFromURL = read . last . splitOn "="

  serviceStatusFromTags :: [Tag String] -> ServiceStatus
  serviceStatusFromTags tags | statusImage == "normal.png"         = Normal
                             | statusImage == "normal-info.png"    = Normal
                             | statusImage == "cancelled.png"      = Cancelled
                             | statusImage == "cancelled-info.png" = Cancelled
                             | statusImage == "affected.png"       = Disrupted
                             | statusImage == "affected-info.png"  = Disrupted
                             | statusImage == "beware.png"         = Disrupted
                             | statusImage == "beware-info.png"    = Disrupted
                             | otherwise                           = Unknown
   where
    statusImage =
      last . splitOn "/" . head . splitOn "?" . fromAttrib "src" $ tags !! 3

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old

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
