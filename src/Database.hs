{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database
  ( getService
  , getServices
  , createInstallation
  , addServiceToInstallation
  , deleteServiceForInstallation
  , getServicesForInstallation
  , getInstallationWithID
  , getIntererestedInstallationsForServiceID
  , saveServices
  )
where

import           Control.Monad                  ( void )
import           Data.ByteString                ( ByteString )
import           Data.Maybe                     ( listToMaybe )
import           Data.Time.Clock                ( UTCTime )
import           Data.String                    ( fromString )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           System.Environment             ( getEnv )

import           Types

connectionString :: IO ByteString
connectionString = fromString <$> getEnv "DB_CONNECTION"

withConnection :: (Connection -> IO a) -> IO a
withConnection action = do
  dbConnection <- connectionString >>= connectPostgreSQL
  result       <- action dbConnection
  close dbConnection
  return result

getService :: Int -> IO (Maybe Service)
getService serviceID = do
  results <- withConnection $ \connection -> query
    connection
    [sql| 
      SELECT service_id, sort_order, area, route, status, additional_info, disruption_reason, last_updated_date, updated 
      FROM services 
      WHERE service_id = ? 
    |]
    (Only serviceID)
  return $ listToMaybe results

getServices :: IO [Service]
getServices = withConnection $ \connection -> query_
  connection
  [sql| 
      SELECT service_id, sort_order, area, route, status, additional_info, disruption_reason, last_updated_date, updated
      FROM services 
    |]

createInstallation :: UUID -> String -> DeviceType -> String -> UTCTime -> IO ()
createInstallation installationID deviceToken deviceType awsSNSEndpointARN time
  = void $ withConnection $ \connection -> execute
    connection
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

addServiceToInstallation :: UUID -> Int -> IO ()
addServiceToInstallation installationID serviceID =
  void $ withConnection $ \connection -> execute
    connection
    [sql|
      INSERT INTO installation_services (installation_id, service_id) 
      VALUES (?,?)
    |]
    (installationID, serviceID)

deleteServiceForInstallation :: UUID -> Int -> IO ()
deleteServiceForInstallation installationID serviceID =
  void $ withConnection $ \connection -> execute
    connection
    [sql|
      DELETE FROM installation_services WHERE installation_id = ? AND service_id = ?
    |]
    (installationID, serviceID)

getServicesForInstallation :: UUID -> IO [Service]
getServicesForInstallation installationID = withConnection $ \connection ->
  query
    connection
    [sql| 
          SELECT s.service_id, s.sort_order, s.area, s.route, s.status, s.additional_info, s.disruption_reason, s.last_updated_date, s.updated 
          FROM services s
          JOIN installation_services i ON s.service_id = i.service_id
          WHERE i.installation_id = ?
        |]
    (Only installationID)

getInstallationWithID :: UUID -> IO (Maybe Installation)
getInstallationWithID installationID = do
  results <- withConnection $ \connection -> query
    connection
    [sql| 
      SELECT i.installation_id, i.device_token, i.device_type, i.endpoint_arn, i.updated 
      FROM installations i 
      WHERE installation_id = ? 
    |]
    (Only installationID)
  return $ listToMaybe results

getIntererestedInstallationsForServiceID :: Int -> IO [Installation]
getIntererestedInstallationsForServiceID serviceID =
  withConnection $ \connection -> query
    connection
    [sql| 
        SELECT i.installation_id, i.device_token, i.device_type, i.endpoint_arn, i.updated
        FROM installation_services s
        JOIN installations i on s.installation_id = i.installation_id
        WHERE s.service_id = ? 
      |]
    (Only $ serviceID)

saveServices :: [Service] -> IO ()
saveServices services = void $ withConnection $ \connection -> executeMany
  connection
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
  services
