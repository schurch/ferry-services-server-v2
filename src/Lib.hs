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
  , fetchStatusesAndNotify
  , AddServiceRequest(..)
  )
where

import           Control.Monad                  ( void
                                                , when
                                                , forM_
                                                )
import           Data.Aeson                     ( eitherDecode
                                                , encode
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Maybe                     ( listToMaybe
                                                , fromMaybe
                                                , isNothing
                                                , fromJust
                                                )
import           Data.String                    ( fromString )
import           Data.Text.Lazy                 ( pack
                                                , unpack
                                                )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Network.HTTP                   ( simpleHTTP
                                                , getRequest
                                                , getResponseBody
                                                )
import           System.Environment             ( getEnv )

import           AWS
import           Types

import qualified Data.ByteString.Lazy.Char8    as C

import           Debug.Trace

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
registerDeviceToken installationID deviceToken deviceType = do
  dbConnection <- connectionString >>= connectPostgreSQL
  result       <- query
    dbConnection
    [sql| SELECT endpoint_arn FROM installations WHERE installation_id = ? |]
    (Only installationID)
  let storedEndpointARN = listToMaybe result
  currentEndpointARN <- if (isNothing storedEndpointARN)
    then createPushEndpoint deviceToken deviceType
    else return $ fromOnly . fromJust $ storedEndpointARN
  endpointAttributesResult <- getAttributesForEndpoint currentEndpointARN
  case endpointAttributesResult of
    EndpointNotFound -> createPushEndpoint deviceToken deviceType
    AttributeResults awsDeviceToken isEnabled -> do
      when (awsDeviceToken /= deviceToken || isEnabled == False)
        $ void
        $ updateDeviceTokenForEndpoint currentEndpointARN deviceToken
      return currentEndpointARN

fetchStatusesAndNotify :: IO ()
fetchStatusesAndNotify = do
  services <- fetchServices
  notifyForServices services
  saveServices services
 where
  notifyForServices :: [Service] -> IO ()
  notifyForServices services = forM_ services $ \service -> do
    savedService <- getService $ serviceID service
    case savedService of
      Just savedService ->
        if serviceStatus service /= serviceStatus savedService
          then do
            let message = serviceToNotificationMessage service
            let payload =
                  pushPayloadWithMessageAndServiceID message (serviceID service)
            dbConnection            <- connectionString >>= connectPostgreSQL
            interestedInstallations <- query
              dbConnection
              [sql| 
                SELECT i.installation_id, i.device_token, i.device_type, i.endpoint_arn, i.updated
                FROM installation_services s
                JOIN installations i on s.installation_id = i.installation_id
                WHERE s.service_id = ? 
              |]
              (Only $ serviceID service)
            forM_ interestedInstallations
              $ \Installation { installationEndpointARN = endpointARN } ->
                  sendNotificationWihPayload endpointARN payload
          else return ()
      Nothing -> return ()
   where
    serviceToNotificationMessage :: Service -> String
    serviceToNotificationMessage Service { serviceRoute = serviceRoute, serviceStatus = serviceStatus }
      | serviceStatus == Normal
      = "Normal services have resumed for " <> serviceRoute
      | serviceStatus == Disrupted
      = "There is a disruption to the service " <> serviceRoute
      | serviceStatus == Cancelled
      = "Sailings have been cancelled for " <> serviceRoute
      | serviceStatus == Unknown
      = error "Do not message for unknow service"

    pushPayloadWithMessageAndServiceID :: String -> Int -> PushPayload
    pushPayloadWithMessageAndServiceID message serviceID =
      let apsPayload    = (APSPayload (APSPayloadBody message) serviceID)
          stringPayload = C.unpack . encode $ apsPayload
      in  PushPayload { pushPayloadDefault     = message
                      , pushPayloadApns        = Just stringPayload
                      , pushPayloadApnsSandbox = Just stringPayload
                      , pushPayloadGcm         = Nothing
                      }

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
