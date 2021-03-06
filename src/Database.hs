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
  , deleteInstallationWithID
  , getServiceLocations
  , getLocationsForServiceID
  ) where

import           Control.Monad                  ( void
                                                , forM_
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Maybe                     ( listToMaybe )
import           Data.Time.Clock                ( UTCTime )
import           Data.String                    ( fromString )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple     ( Connection
                                                , execute
                                                , execute_
                                                , executeMany
                                                , query
                                                , query_
                                                , close
                                                , connectPostgreSQL
                                                , Only(Only)
                                                , withTransaction
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..) )
import           System.Environment             ( getEnv )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Types                          ( ServiceLocation
                                                , Installation
                                                , Service
                                                , DeviceType
                                                , Location
                                                )
import           Data.Time.LocalTime            ( TimeOfDay(..) )
import           Utility                        ( splitOn )
import           Data.Time.Calendar             ( Day )

connectionString :: IO ByteString
connectionString = fromString <$> getEnv "DB_CONNECTION"

withConnection :: MonadIO m => (Connection -> IO a) -> m a
withConnection action = do
  dbConnection <- liftIO $ connectionString >>= connectPostgreSQL
  result       <- liftIO $ action dbConnection
  liftIO $ close dbConnection
  return result

getService :: MonadIO m => Int -> m (Maybe Types.Service)
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

getServices :: MonadIO m => m [Types.Service]
getServices = withConnection $ \connection -> query_
  connection
  [sql| 
    SELECT service_id, sort_order, area, route, status, additional_info, disruption_reason, last_updated_date, updated
    FROM services 
  |]

createInstallation
  :: MonadIO m => UUID -> String -> DeviceType -> String -> UTCTime -> m ()
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

addServiceToInstallation :: MonadIO m => UUID -> Int -> m ()
addServiceToInstallation installationID serviceID =
  void $ withConnection $ \connection -> execute
    connection
    [sql|
      INSERT INTO installation_services (installation_id, service_id) 
      VALUES (?,?)
      ON CONFLICT DO NOTHING
    |]
    (installationID, serviceID)

deleteServiceForInstallation :: MonadIO m => UUID -> Int -> m ()
deleteServiceForInstallation installationID serviceID =
  void $ withConnection $ \connection -> execute
    connection
    [sql|
      DELETE FROM installation_services WHERE installation_id = ? AND service_id = ?
    |]
    (installationID, serviceID)

getServicesForInstallation :: MonadIO m => UUID -> m [Types.Service]
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

getInstallationWithID :: MonadIO m => UUID -> m (Maybe Installation)
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

getIntererestedInstallationsForServiceID
  :: MonadIO m => Int -> m [Installation]
getIntererestedInstallationsForServiceID serviceID =
  withConnection $ \connection -> query
    connection
    [sql| 
      SELECT i.installation_id, i.device_token, i.device_type, i.endpoint_arn, i.updated
      FROM installation_services s
      JOIN installations i ON s.installation_id = i.installation_id
      WHERE s.service_id = ? 
      |]
    (Only serviceID)

saveServices :: MonadIO m => [Types.Service] -> m ()
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

deleteInstallationWithID :: MonadIO m => UUID -> m ()
deleteInstallationWithID installationID = do
  deleteInstallationServicesWithID installationID
  void $ withConnection $ \connection -> execute
    connection
    [sql| 
      DELETE FROM installations WHERE installation_id = ?
    |]
    (Only installationID)

deleteInstallationServicesWithID :: MonadIO m => UUID -> m ()
deleteInstallationServicesWithID installationID =
  void $ withConnection $ \connection -> execute
    connection
    [sql| 
      DELETE FROM installation_services WHERE installation_id = ?
    |]
    (Only installationID)

getServiceLocations :: MonadIO m => m [ServiceLocation]
getServiceLocations = withConnection $ \connection -> query_
  connection
  [sql| 
    SELECT sl.service_id, l.location_id, l.name, l.latitude, l.longitude
    FROM service_locations sl
    JOIN locations l ON l.location_id = sl.location_id 
  |]

getLocationsForServiceID :: MonadIO m => Int -> m [Location]
getLocationsForServiceID serviceID = withConnection $ \connection -> query
  connection
  [sql| 
    SELECT l.location_id, l.name, l.latitude, l.longitude
    FROM service_locations sl
    JOIN locations l ON l.location_id = sl.location_id 
    WHERE sl.service_id = ?
  |]
  (Only serviceID)
