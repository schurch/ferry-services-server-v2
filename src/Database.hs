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
import           Control.Monad.Reader             (asks)
import           Data.Maybe                       (listToMaybe)
import           Data.Pool                        (Pool, withResource)
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

withConnection :: (Connection -> IO a) -> Application a
withConnection action = do
  connectionPool <- asks connectionPool
  liftIO $ withResource connectionPool action

insertLocationWeather :: Int -> WeatherFetcherResult -> Application ()
insertLocationWeather locationID
  (WeatherFetcherResult (
    (WeatherFetcherResultWeather icon description) : _)
    (WeatherFetcherResultMain temperature)
    (WeatherFetcherResultWind windSpeed windDirection)) = withConnection $ \connection -> void $ execute
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
insertLocationWeather _ _ = return ()

getLocationWeathers :: Application [LocationWeather]
getLocationWeathers = withConnection $ \connection -> query_
  connection
  [sql|
    SELECT location_id, description, icon, temperature, wind_speed, wind_direction, updated, created
    FROM location_weather
  |]

getService :: Int -> Application (Maybe Types.Service)
getService serviceID = do
  results <- withConnection $ \connection -> query
    connection
    [sql|
      SELECT service_id, area, route, status, additional_info, disruption_reason, organisation_id, last_updated_date, updated
      FROM services
      WHERE service_id = ? AND visible = TRUE
    |]
    (Only serviceID)
  return $ listToMaybe results

getServices :: Application [Types.Service]
getServices = withConnection $ \connection -> query_
  connection
  [sql|
    SELECT service_id, area, route, status, additional_info, disruption_reason, organisation_id, last_updated_date, updated
    FROM services
    WHERE visible = TRUE
    ORDER BY area, route
  |]

getServicesForOrganisation :: Int -> Application [Types.Service]
getServicesForOrganisation organisationID = withConnection $ \connection -> query connection
  [sql|
    SELECT service_id, area, route, status, additional_info, disruption_reason, organisation_id, last_updated_date, updated
    FROM services
    WHERE organisation_id = ?
    ORDER BY area, route
  |]
  (Only organisationID)

hideServicesWithIDs :: [Int] -> Application ()
hideServicesWithIDs serviceIDs = withConnection $ \connection -> void $ execute
  connection
  [sql|
    UPDATE services SET visible = FALSE WHERE service_id IN ?
  |]
  (Only $ In serviceIDs)

createInstallation
  :: UUID -> String -> DeviceType -> String -> UTCTime -> Application ()
createInstallation installationID deviceToken deviceType awsSNSEndpointARN time
  = withConnection $ \connection -> void $ execute
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

addServiceToInstallation :: UUID -> Int -> Application ()
addServiceToInstallation installationID serviceID =
  withConnection $ \connection -> void $ execute
    connection
    [sql|
      INSERT INTO installation_services (installation_id, service_id)
      VALUES (?,?)
      ON CONFLICT DO NOTHING
    |]
    (installationID, serviceID)

deleteServiceForInstallation :: UUID -> Int -> Application ()
deleteServiceForInstallation installationID serviceID =
  withConnection $ \connection -> void $ execute
    connection
    [sql|
      DELETE FROM installation_services WHERE installation_id = ? AND service_id = ?
    |]
    (installationID, serviceID)

getServicesForInstallation :: UUID -> Application [Types.Service]
getServicesForInstallation installationID =
  withConnection $ \connection -> query
    connection
    [sql|
      SELECT s.service_id, s.area, s.route, s.status, s.additional_info, s.disruption_reason, s.organisation_id, s.last_updated_date, s.updated
      FROM services s
      JOIN installation_services i ON s.service_id = i.service_id
      WHERE i.installation_id = ? AND s.visible = TRUE
      ORDER BY s.area, s.route
    |]
    (Only installationID)

getInstallationWithID :: UUID -> Application (Maybe Installation)
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
  :: Int -> Application [Installation]
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

saveServices :: [Types.Service] -> Application ()
saveServices services = withConnection $ \connection -> void $ do
  executeMany
    connection
    [sql|
      INSERT INTO services (service_id, area, route, status, additional_info, disruption_reason, organisation_id, last_updated_date, updated)
      VALUES (?,?,?,?,?,?,?,?,?)
      ON CONFLICT (service_id) DO UPDATE
        SET service_id = excluded.service_id,
            area = excluded.area,
            route = excluded.route,
            status = excluded.status,
            additional_info = excluded.additional_info,
            disruption_reason = excluded.disruption_reason,
            organisation_id = excluded.organisation_id,
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

deleteInstallationWithID :: UUID -> Application ()
deleteInstallationWithID installationID = do
  deleteInstallationServicesWithID installationID
  withConnection $ \connection -> void $ execute
    connection
    [sql|
      DELETE FROM installations WHERE installation_id = ?
    |]
    (Only installationID)

deleteInstallationServicesWithID :: UUID -> Application ()
deleteInstallationServicesWithID installationID =
  withConnection $ \connection -> void $ execute
    connection
    [sql|
      DELETE FROM installation_services WHERE installation_id = ?
    |]
    (Only installationID)

getServiceLocations :: Application [ServiceLocation]
getServiceLocations = withConnection $ \connection -> query_
  connection
  [sql|
    SELECT sl.service_id, l.location_id, l.name, l.coordinate
    FROM service_locations sl
    JOIN locations l ON l.location_id = sl.location_id
  |]

getLocations :: Application [Location]
getLocations = withConnection $ \connection -> query_
  connection
  [sql|
    SELECT location_id, name, coordinate, created
    FROM locations
  |]

saveVessel :: Vessel -> Application ()
saveVessel vessel = withConnection $ \connection -> void $ do
  execute
    connection
    [sql|
      INSERT INTO vessels (mmsi, name, speed, course, coordinate, last_received, updated, organisation_id)
        VALUES (?,?,?,?,?,?,?,?)
        ON CONFLICT (mmsi) DO UPDATE
          SET name = excluded.name,
              speed = excluded.speed,
              course = excluded.course,
              coordinate = excluded.coordinate,
              last_received = excluded.last_received,
              updated = excluded.updated,
              organisation_id = excluded.organisation_id
    |] vessel

getVessels :: Application [Vessel]
getVessels = withConnection $ \connection -> query_
  connection
  [sql|
    SELECT mmsi, name, speed, course, coordinate, last_received, updated, organisation_id
    FROM vessels
  |]

getServiceVessels :: Application [ServiceVessel]
getServiceVessels = withConnection $ \connection -> query_
  connection
  [sql|
    WITH bounding_box AS (
      SELECT sl.service_id, ST_Expand(ST_Extent(l.coordinate), 0.01) AS bounds
      FROM locations l
      JOIN service_locations sl ON l.location_id = sl.location_id
      GROUP BY sl.service_id
    )
    SELECT s.service_id, v.mmsi, v.name, v.speed, v.course, v.coordinate, v.last_received, v.updated, v.organisation_id
    FROM vessels v, bounding_box b
    JOIN services s on s.service_id = b.service_id
    WHERE v.coordinate && b.bounds AND s.organisation_id = v.organisation_id;
  |]
