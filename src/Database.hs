{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Database
  ( insertLocationWeather
  , getLocationWeathers
  , getService
  , getServices
  , getServicesForOrganisation
  , hideServicesWithIDs
  , createInstallation
  , addServiceToInstallation
  , deleteServiceForInstallation
  , getServicesForInstallation
  , getInstallationWithID
  , getIntererestedInstallationsForServiceID
  , saveServices
  , deleteInstallationWithID
  , getServiceLocations
  , getLocations
  , saveVessel
  , getVessels
  , getServiceVessels
  ) where

import           Control.Monad                    (forM_, void)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Maybe                       (listToMaybe)
import           Data.Time.Calendar               (Day)
import           Data.Time.Clock                  (UTCTime)
import           Data.Time.LocalTime              (TimeOfDay (..))
import           Data.UUID                        (UUID)
import           Database.PostgreSQL.Simple       (Connection, In (In),
                                                   Only (Only), execute,
                                                   executeMany, execute_, query,
                                                   query_, withTransaction)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (PGArray (..))
import           System.Environment               (getEnv)
import           Types
import           Utility                          (splitOn)

insertLocationWeather :: MonadIO m => Connection -> Int -> WeatherFetcherResult -> m ()
insertLocationWeather connection locationID
  (WeatherFetcherResult (
    (WeatherFetcherResultWeather icon description) : _)
    (WeatherFetcherResultMain temperature)
    (WeatherFetcherResultWind windSpeed windDirection)) = liftIO $ void $ execute
      connection
      [sql|
        INSERT INTO location_weather (location_id, description, icon, temperature, wind_speed, wind_direction)
          VALUES (?,?,?,?,?,?)
          ON CONFLICT (location_id) DO UPDATE
            SET description = excluded.description,
                icon = excluded.icon,
                temperature = excluded.temperature,
                wind_speed = excluded.wind_speed,
                wind_direction = excluded.wind_direction,
                updated = CURRENT_TIMESTAMP
      |]
      (locationID, description, icon, temperature, windSpeed, windDirection)
insertLocationWeather _ _ _ = return ()

getLocationWeathers :: MonadIO m => Connection -> m [LocationWeather]
getLocationWeathers connection = liftIO $ query_
  connection
  [sql|
    SELECT location_id, description, icon, temperature, wind_speed, wind_direction, updated, created
    FROM location_weather
  |]

getService :: MonadIO m => Connection -> Int -> m (Maybe Types.Service)
getService connection serviceID = do
  results <- liftIO $ query
    connection
    [sql|
      SELECT service_id, area, route, status, additional_info, disruption_reason, organisation, last_updated_date, updated
      FROM services
      WHERE service_id = ? AND visible = TRUE
    |]
    (Only serviceID)
  return $ listToMaybe results

getServices :: MonadIO m => Connection -> m [Types.Service]
getServices connection = liftIO $ query_
  connection
  [sql|
    SELECT service_id, area, route, status, additional_info, disruption_reason, organisation, last_updated_date, updated
    FROM services
    WHERE visible = TRUE
    ORDER BY area, route
  |]

getServicesForOrganisation :: MonadIO m => Connection -> String -> m [Types.Service]
getServicesForOrganisation connection organisation = liftIO $ query connection
  [sql|
    SELECT service_id, area, route, status, additional_info, disruption_reason, organisation, last_updated_date, updated
    FROM services
    WHERE organisation = ?
    ORDER BY area, route
  |]
  (Only organisation)

hideServicesWithIDs :: MonadIO m => Connection -> [Int] -> m ()
hideServicesWithIDs connection serviceIDs = liftIO $ void $ execute
  connection
  [sql|
    UPDATE services SET visible = FALSE WHERE service_id IN ?
  |]
  (Only $ In serviceIDs)

createInstallation
  :: MonadIO m => Connection -> UUID -> String -> DeviceType -> String -> UTCTime -> m ()
createInstallation connection installationID deviceToken deviceType awsSNSEndpointARN time
  = liftIO $ void $ execute
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

addServiceToInstallation :: MonadIO m => Connection -> UUID -> Int -> m ()
addServiceToInstallation connection installationID serviceID =
  liftIO $ void $ execute
    connection
    [sql|
      INSERT INTO installation_services (installation_id, service_id)
      VALUES (?,?)
      ON CONFLICT DO NOTHING
    |]
    (installationID, serviceID)

deleteServiceForInstallation :: MonadIO m => Connection -> UUID -> Int -> m ()
deleteServiceForInstallation connection installationID serviceID =
  liftIO $ void $ execute
    connection
    [sql|
      DELETE FROM installation_services WHERE installation_id = ? AND service_id = ?
    |]
    (installationID, serviceID)

getServicesForInstallation :: MonadIO m => Connection -> UUID -> m [Types.Service]
getServicesForInstallation connection installationID =
  liftIO $ query
    connection
    [sql|
      SELECT s.service_id, s.area, s.route, s.status, s.additional_info, s.disruption_reason, s.organisation, s.last_updated_date, s.updated
      FROM services s
      JOIN installation_services i ON s.service_id = i.service_id
      WHERE i.installation_id = ? AND s.visible = TRUE
      ORDER BY s.area, s.route
    |]
    (Only installationID)

getInstallationWithID :: MonadIO m => Connection -> UUID -> m (Maybe Installation)
getInstallationWithID connection installationID = do
  results <- liftIO $ query
    connection
    [sql|
      SELECT i.installation_id, i.device_token, i.device_type, i.endpoint_arn, i.updated
      FROM installations i
      WHERE installation_id = ?
    |]
    (Only installationID)
  return $ listToMaybe results

getIntererestedInstallationsForServiceID
  :: MonadIO m => Connection -> Int -> m [Installation]
getIntererestedInstallationsForServiceID connection serviceID =
  liftIO $ query
    connection
    [sql|
      SELECT i.installation_id, i.device_token, i.device_type, i.endpoint_arn, i.updated
      FROM installation_services s
      JOIN installations i ON s.installation_id = i.installation_id
      WHERE s.service_id = ?
    |]
    (Only serviceID)

saveServices :: MonadIO m => Connection -> [Types.Service] -> m ()
saveServices connection services = liftIO $ void $ do
  executeMany
    connection
    [sql|
      INSERT INTO services (service_id, area, route, status, additional_info, disruption_reason, organisation, last_updated_date, updated)
      VALUES (?,?,?,?,?,?,?,?,?)
      ON CONFLICT (service_id) DO UPDATE
        SET service_id = excluded.service_id,
            area = excluded.area,
            route = excluded.route,
            status = excluded.status,
            additional_info = excluded.additional_info,
            disruption_reason = excluded.disruption_reason,
            organisation = excluded.organisation,
            last_updated_date = excluded.last_updated_date,
            updated = excluded.updated
    |]
    services
  let serviceIDs = serviceID <$> services
  execute
    connection
    [sql|
      UPDATE services SET visible = TRUE WHERE service_id IN ?
    |]
    (Only $ In serviceIDs)

deleteInstallationWithID :: MonadIO m => Connection -> UUID -> m ()
deleteInstallationWithID connection installationID = do
  deleteInstallationServicesWithID connection installationID
  liftIO $ void $ execute
    connection
    [sql|
      DELETE FROM installations WHERE installation_id = ?
    |]
    (Only installationID)

deleteInstallationServicesWithID :: MonadIO m => Connection -> UUID -> m ()
deleteInstallationServicesWithID connection installationID =
  liftIO $ void $ execute
    connection
    [sql|
      DELETE FROM installation_services WHERE installation_id = ?
    |]
    (Only installationID)

getServiceLocations :: MonadIO m => Connection -> m [ServiceLocation]
getServiceLocations connection = liftIO $ query_
  connection
  [sql|
    SELECT sl.service_id, l.location_id, l.name, l.coordinate
    FROM service_locations sl
    JOIN locations l ON l.location_id = sl.location_id
  |]

getLocations :: MonadIO m => Connection -> m [Location]
getLocations connection = liftIO $ query_
  connection
  [sql|
    SELECT location_id, name, coordinate, created
    FROM locations
  |]

saveVessel :: MonadIO m => Connection -> Vessel -> m ()
saveVessel connection vessel = liftIO $ void $ do
  execute
    connection
    [sql|
      INSERT INTO vessels (mmsi, name, speed, course, coordinate, last_received, updated)
        VALUES (?,?,?,?,?,?,?)
        ON CONFLICT (mmsi) DO UPDATE
          SET name = excluded.name,
              speed = excluded.speed,
              course = excluded.course,
              coordinate = excluded.coordinate,
              last_received = excluded.last_received,
              updated = excluded.updated
    |] vessel

getVessels :: MonadIO m => Connection -> m [Vessel]
getVessels connection = liftIO $ query_
  connection
  [sql|
    SELECT mmsi, name, speed, course, coordinate, last_received, updated
    FROM vessels
  |]

getServiceVessels :: MonadIO m => Connection -> m [ServiceVessel]
getServiceVessels connection = liftIO $ query_
  connection
  [sql|
    WITH bounding_box AS (
      SELECT sl.service_id, ST_Expand(ST_Extent(l.coordinate), 0.01) AS bounds
      FROM locations l
      JOIN service_locations sl ON l.location_id = sl.location_id
      GROUP BY sl.service_id
    )
    SELECT b.service_id, v.mmsi, v.name, v.speed, v.course, v.coordinate, v.last_received, v.updated
    FROM vessels v, bounding_box b
    WHERE v.coordinate && b.bounds;
  |]
