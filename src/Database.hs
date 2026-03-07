{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Database
  ( insertRailDepartureFetcherResult,
    getLocationRailDepartures,
    insertLocationWeather,
    getLocationWeathers,
    getService,
    getServices,
    getServicesForOrganisation,
    hideServicesWithIDs,
    createInstallation,
    addServiceToInstallation,
    deleteServiceForInstallation,
    getServicesForInstallation,
    updatePushEnabled,
    getInstallationWithID,
    getIntererestedInstallationsForServiceID,
    saveServices,
    deleteInstallationWithID,
    getServiceLocations,
    getLocations,
    saveVessel,
    getVessels,
    getServiceVessels,
    getLocationDeparturesV2,
    getServicesWithScheduledDeparturesV2,
    getServiceOrganisations,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import Data.Maybe (listToMaybe)
import Data.Pool (Pool, withResource)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeOfDay (..))
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
  ( Connection,
    In (In),
    Only (Only),
    execute,
    executeMany,
    execute_,
    query,
    query_,
    withTransaction,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import System.Environment (getEnv)
import Types
import Utility (splitOn)

withConnection :: (Connection -> IO a) -> Application a
withConnection action = do
  connectionPool <- asks connectionPool
  liftIO $ withResource connectionPool action

insertRailDepartureFetcherResult :: RailDepartureFetcherResult -> Int -> Application ()
insertRailDepartureFetcherResult railDeparturesFetcherResult locationID = withConnection $ \connection ->
  withTransaction connection $ do
    deleteOldData connection railDeparturesFetcherResult
    insertRailDepartures connection railDeparturesFetcherResult
  where
    insertRailDepartures :: Connection -> RailDepartureFetcherResult -> IO ()
    insertRailDepartures connection railDeparturesResult = do
      let statement =
            [sql| 
              INSERT INTO rail_departures (departure_crs, departure_name, destination_crs, destination_name, scheduled_departure_time, estimated_departure_time, cancelled, platform, location_id) 
              VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            |]
      void $ executeMany connection statement (convertToRailDepartures railDeparturesResult locationID)

    convertToRailDepartures :: RailDepartureFetcherResult -> Int -> [(String, String, String, String, TimeOfDay, String, Bool, Maybe String, Int)]
    convertToRailDepartures RailDepartureFetcherResult {..} locationID =
      let departureCRS = railDepartureFetcherResultCrs
          departureName = railDepartureFetcherResultLocationName
       in case railDepartureFetcherResultTrainServices of
            Just trainServices -> convertToRailDeparture departureCRS departureName locationID <$> filter (isValidService departureCRS) trainServices
            Nothing -> []

    convertToRailDeparture :: String -> String -> Int -> RailDepartureFetcherTrainService -> (String, String, String, String, TimeOfDay, String, Bool, Maybe String, Int)
    convertToRailDeparture departureCRS departureName locationID trainService =
      ( departureCRS,
        departureName,
        railDepartureFetcherLocationCrs . head . railDepartureFetcherTrainServiceDestination $ trainService,
        railDepartureFetcherLocationLocationName . head . railDepartureFetcherTrainServiceDestination $ trainService,
        timeStringToTime . railDepartureFetcherTrainServiceStd $ trainService,
        railDepartureFetcherTrainServiceEtd trainService,
        railDepartureFetcherTrainServiceIsCancelled trainService,
        railDepartureFetcherTrainServicePlatform trainService,
        locationID
      )

    isValidService :: String -> RailDepartureFetcherTrainService -> Bool
    isValidService departureCRS trainService =
      let currentDestination = railDepartureFetcherLocationCrs . head <$> railDepartureFetcherTrainServiceCurrentDestinations trainService
       in currentDestination /= Just departureCRS

    deleteOldData :: Connection -> RailDepartureFetcherResult -> IO ()
    deleteOldData connection railDeparturesFetcherResult = do
      void $
        execute
          connection
          [sql| 
            DELETE FROM rail_departures WHERE departure_crs = ?
          |]
          (Only $ railDepartureFetcherResultCrs railDeparturesFetcherResult)

getLocationRailDepartures :: Day -> Application [LocationRailDeparture]
getLocationRailDepartures date = withConnection $ \connection ->
  query
    connection
    [sql|
      WITH constants(query_date) AS (
          VALUES (date ?)
      )
      SELECT
        location_id,
        departure_crs,
        departure_name,
        destination_crs,
        destination_name,
        (
          (query_date + scheduled_departure_time) AT TIME ZONE 'Europe/London' AT TIME ZONE 'UTC'
        ) :: TIMESTAMP AS scheduled_departure_time,
        estimated_departure_time,
        cancelled,
        platform
      FROM 
        rail_departures,
        constants
      WHERE
        (current_timestamp - created) < '5 minutes'
    |]
    (Only date)

insertLocationWeather :: Int -> WeatherFetcherResult -> Application ()
insertLocationWeather
  locationID
  ( WeatherFetcherResult
      ((WeatherFetcherResultWeather icon description) : _)
      (WeatherFetcherResultMain temperature)
      (WeatherFetcherResultWind windSpeed windDirection)
    ) = withConnection $ \connection ->
    void $
      execute
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
getLocationWeathers = withConnection $ \connection ->
  query_
    connection
    [sql|
      SELECT location_id, description, icon, temperature, wind_speed, wind_direction, updated, created
      FROM location_weather
    |]

getService :: Int -> Application (Maybe Types.Service)
getService serviceID = do
  results <- withConnection $ \connection ->
    query
      connection
      [sql|
        SELECT service_id, area, route, status, additional_info, disruption_reason, organisation_id, last_updated_date, updated
        FROM services
        WHERE service_id = ? AND visible = TRUE
      |]
      (Only serviceID)
  return $ listToMaybe results

getServices :: Application [Types.Service]
getServices = withConnection $ \connection ->
  query_
    connection
    [sql|
      SELECT service_id, area, route, status, additional_info, disruption_reason, organisation_id, last_updated_date, updated
      FROM services
      WHERE visible = TRUE
      ORDER BY area, route
    |]

getServicesForOrganisation :: Int -> Application [Types.Service]
getServicesForOrganisation organisationID = withConnection $ \connection ->
  query
    connection
    [sql|
      SELECT service_id, area, route, status, additional_info, disruption_reason, organisation_id, last_updated_date, updated
      FROM services
      WHERE organisation_id = ?
      ORDER BY area, route
    |]
    (Only organisationID)

hideServicesWithIDs :: [Int] -> Application ()
hideServicesWithIDs serviceIDs = withConnection $ \connection ->
  void $
    execute
      connection
      [sql|
        UPDATE services SET visible = FALSE WHERE service_id IN ?
      |]
      (Only $ In serviceIDs)

createInstallation ::
  UUID -> String -> DeviceType -> String -> UTCTime -> Application ()
createInstallation installationID deviceToken deviceType awsSNSEndpointARN time =
  withConnection $ \connection ->
    void $
      execute
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
  withConnection $ \connection ->
    void $
      execute
        connection
        [sql|
          INSERT INTO installation_services (installation_id, service_id)
          VALUES (?,?)
          ON CONFLICT DO NOTHING
        |]
        (installationID, serviceID)

deleteServiceForInstallation :: UUID -> Int -> Application ()
deleteServiceForInstallation installationID serviceID =
  withConnection $ \connection ->
    void $
      execute
        connection
        [sql|
          DELETE FROM installation_services WHERE installation_id = ? AND service_id = ?
        |]
        (installationID, serviceID)

getServicesForInstallation :: UUID -> Application [Types.Service]
getServicesForInstallation installationID =
  withConnection $ \connection ->
    query
      connection
      [sql|
        SELECT s.service_id, s.area, s.route, s.status, s.additional_info, s.disruption_reason, s.organisation_id, s.last_updated_date, s.updated
        FROM services s
        JOIN installation_services i ON s.service_id = i.service_id
        WHERE i.installation_id = ? AND s.visible = TRUE
        ORDER BY s.area, s.route
      |]
      (Only installationID)

updatePushEnabled :: UUID -> Bool -> Application ()
updatePushEnabled installationID pushEnabled = do
  withConnection $ \connection ->
    void $
      execute
        connection
        [sql|
          UPDATE Installations SET push_enabled = ? WHERE installation_id = ?
        |]
        (pushEnabled, installationID)

getInstallationWithID :: UUID -> Application (Maybe Installation)
getInstallationWithID installationID = do
  results <- withConnection $ \connection ->
    query
      connection
      [sql|
        SELECT i.installation_id, i.device_token, i.device_type, i.endpoint_arn, i.push_enabled, i.updated
        FROM installations i
        WHERE installation_id = ?
      |]
      (Only installationID)
  return $ listToMaybe results

getIntererestedInstallationsForServiceID ::
  Int -> Application [Installation]
getIntererestedInstallationsForServiceID serviceID =
  withConnection $ \connection ->
    query
      connection
      [sql|
        SELECT i.installation_id, i.device_token, i.device_type, i.endpoint_arn, i.push_enabled, i.updated
        FROM installation_services s
        JOIN installations i ON s.installation_id = i.installation_id
        WHERE s.service_id = ? AND i.push_enabled = TRUE
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
  withConnection $ \connection ->
    void $
      execute
        connection
        [sql|
          DELETE FROM installations WHERE installation_id = ?
        |]
        (Only installationID)

deleteInstallationServicesWithID :: UUID -> Application ()
deleteInstallationServicesWithID installationID =
  withConnection $ \connection ->
    void $
      execute
        connection
        [sql|
          DELETE FROM installation_services WHERE installation_id = ?
        |]
        (Only installationID)

getServiceLocations :: Application [ServiceLocation]
getServiceLocations = withConnection $ \connection ->
  query_
    connection
    [sql|
      SELECT sl.service_id, l.location_id, l.name, l.coordinate
      FROM service_locations sl
      JOIN locations l ON l.location_id = sl.location_id
    |]

getLocations :: Application [Location]
getLocations = withConnection $ \connection ->
  query_
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
    |]
    vessel

getVessels :: Application [Vessel]
getVessels = withConnection $ \connection ->
  query_
    connection
    [sql|
      SELECT mmsi, name, speed, course, coordinate, last_received, updated, organisation_id
      FROM vessels
    |]

getServiceVessels :: Application [ServiceVessel]
getServiceVessels = withConnection $ \connection ->
  query_
    connection
    [sql|
      WITH bounding_box AS (
        SELECT sl.service_id, ST_Expand(ST_Extent(l.coordinate), 0.02) AS bounds
        FROM locations l
        JOIN service_locations sl ON l.location_id = sl.location_id
        GROUP BY sl.service_id
      )
      SELECT s.service_id, v.mmsi, v.name, v.speed, v.course, v.coordinate, v.last_received, v.updated, v.organisation_id
      FROM vessels v, bounding_box b
      JOIN services s on s.service_id = b.service_id
      WHERE v.coordinate && b.bounds AND s.organisation_id = v.organisation_id;
    |]

getLocationDeparturesV2 :: Int -> Day -> Application [LocationDeparture]
getLocationDeparturesV2 serviceID date = withConnection $ \connection ->
  query
    connection
    [sql|
      WITH constants(query_date) AS (
          VALUES (date ?)
      ),
      timings AS (
          SELECT
            document_id,
            journey_pattern_timing_link_id,
            journey_pattern_section_ref,
            sort_order,
            CASE from_wait_time WHEN ''
                THEN make_interval()
                ELSE make_interval(
                        hours := COALESCE(NULLIF((REGEXP_SPLIT_TO_ARRAY(replace(from_wait_time, 'PT', ''), 'H|M|S'))[1], '') :: Int, 0),
                        mins := COALESCE(NULLIF((REGEXP_SPLIT_TO_ARRAY(replace(from_wait_time, 'PT', ''), 'H|M|S'))[2], '') :: Int, 0),
                        secs := COALESCE(NULLIF((REGEXP_SPLIT_TO_ARRAY(replace(from_wait_time, 'PT', ''), 'H|M|S'))[3], '') :: Int, 0)
                    )
            END AS wait_time,
            CASE run_time WHEN ''
                THEN make_interval()
                ELSE make_interval(
                        hours := COALESCE(NULLIF((REGEXP_SPLIT_TO_ARRAY(replace(run_time, 'PT', ''), 'H|M|S'))[1], '') :: Int, 0),
                        mins := COALESCE(NULLIF((REGEXP_SPLIT_TO_ARRAY(replace(run_time, 'PT', ''), 'H|M|S'))[2], '') :: Int, 0),
                        secs := COALESCE(NULLIF((REGEXP_SPLIT_TO_ARRAY(replace(run_time, 'PT', ''), 'H|M|S'))[3], '') :: Int, 0)
                    )
            END AS run_time
          FROM
              tx2_journey_pattern_timing_links
      ),
      day_of_week(derived_day_of_week) AS (
          SELECT
              CASE
                  WHEN extract(dow from query_date) = 0 THEN 'sunday'
                  WHEN extract(dow from query_date) = 1 THEN 'monday'
                  WHEN extract(dow from query_date) = 2 THEN 'tuesday'
                  WHEN extract(dow from query_date) = 3 THEN 'wednesday'
                  WHEN extract(dow from query_date) = 4 THEN 'thursday'
                  WHEN extract(dow from query_date) = 5 THEN 'friday'
                  WHEN extract(dow from query_date) = 6 THEN 'saturday'
              END
          FROM constants
      ),
      mapped_service_codes AS (
          SELECT DISTINCT service_code
          FROM tx2_service_mappings
          WHERE service_id = ?
      ),
      selected_service AS (
          SELECT route
          FROM services
          WHERE service_id = ?
      ),
      service_stop_points AS (
          SELECT l.location_id, l.name, l.coordinate, l.stop_point_id
          FROM service_locations sl
          JOIN locations l ON l.location_id = sl.location_id
          WHERE sl.service_id = ?
            AND l.stop_point_id IS NOT NULL
      ),
      heuristic_service_codes AS (
          SELECT DISTINCT s.service_code
          FROM selected_service ss
          JOIN service_stop_points sp_from ON TRUE
          JOIN service_stop_points sp_to
            ON sp_to.stop_point_id <> sp_from.stop_point_id
          JOIN tx2_journey_pattern_timing_links jptl
            ON jptl.from_stop_point_ref = sp_from.stop_point_id
           AND jptl.to_stop_point_ref = sp_to.stop_point_id
          JOIN tx2_journey_pattern_sections jps
            ON jps.document_id = jptl.document_id
           AND jps.section_ref = jptl.journey_pattern_section_ref
          JOIN tx2_journey_patterns jp
            ON jp.document_id = jps.document_id
           AND jp.journey_pattern_id = jps.journey_pattern_id
          JOIN tx2_services s
            ON s.document_id = jp.document_id
           AND s.service_code = jp.service_code
          WHERE s.mode = 'ferry'
            AND lower(ss.route) NOT LIKE '%freight%'
      ),
      effective_service_codes AS (
          SELECT service_code
          FROM mapped_service_codes
          UNION
          SELECT service_code
          FROM heuristic_service_codes
          WHERE NOT EXISTS (SELECT 1 FROM mapped_service_codes)
      ),
      pattern_links AS (
          SELECT
              jps.document_id,
              jps.journey_pattern_id,
              jps.section_order,
              t.sort_order,
              ((jps.section_order - 1) * 1000) + t.sort_order AS global_sort_order,
              jptl.from_stop_point_ref,
              jptl.to_stop_point_ref,
              t.wait_time,
              t.run_time
          FROM tx2_journey_pattern_sections jps
          JOIN tx2_journey_pattern_timing_links jptl
            ON jptl.document_id = jps.document_id
           AND jptl.journey_pattern_section_ref = jps.section_ref
          JOIN timings t
            ON t.document_id = jptl.document_id
           AND t.journey_pattern_timing_link_id = jptl.journey_pattern_timing_link_id
      ),
      multi_journey_time AS (
          SELECT
              pl.document_id,
              pl.journey_pattern_id,
              pl.global_sort_order,
              LAG(pl.run_time) OVER (
                  PARTITION BY pl.document_id, pl.journey_pattern_id
                  ORDER BY pl.global_sort_order
              ) + pl.wait_time AS time
          FROM pattern_links pl
      ),
      journey_legs AS (
          SELECT
              vj.document_id,
              vj.journey_pattern_id,
              vj.vehicle_journey_code,
              pl.global_sort_order,
              pl.from_stop_point_ref,
              pl.to_stop_point_ref,
              (
                  (
                      query_date +
                      COALESCE(
                          SUM(mjt.time) OVER (
                              PARTITION BY vj.document_id, vj.journey_pattern_id, vj.vehicle_journey_code
                              ORDER BY pl.global_sort_order
                          ) + vj.departure_time,
                          vj.departure_time
                      )
                  ) AT TIME ZONE 'Europe/London' AT TIME ZONE 'UTC'
              ) :: TIMESTAMP AS departure,
              (
                  (
                      query_date +
                      COALESCE(
                          SUM(mjt.time) OVER (
                              PARTITION BY vj.document_id, vj.journey_pattern_id, vj.vehicle_journey_code
                              ORDER BY pl.global_sort_order
                          ) + vj.departure_time,
                          vj.departure_time
                      ) + pl.run_time
                  ) AT TIME ZONE 'Europe/London' AT TIME ZONE 'UTC'
              ) :: TIMESTAMP AS arrival,
              NULLIF(vj.note, '') AS notes,
              d.source_modification_datetime
          FROM tx2_vehicle_journeys vj
          CROSS JOIN constants
          CROSS JOIN day_of_week
          INNER JOIN pattern_links pl
              ON pl.document_id = vj.document_id
             AND pl.journey_pattern_id = vj.journey_pattern_id
          INNER JOIN multi_journey_time mjt
              ON mjt.document_id = pl.document_id
             AND mjt.journey_pattern_id = pl.journey_pattern_id
             AND mjt.global_sort_order = pl.global_sort_order
          INNER JOIN tx2_services s
              ON s.document_id = vj.document_id
             AND s.service_code = vj.service_code
          INNER JOIN effective_service_codes esc
              ON esc.service_code = s.service_code
          INNER JOIN tx2_documents d
              ON d.document_id = s.document_id
          WHERE s.mode = 'ferry'
            AND (s.start_date IS NULL OR query_date >= s.start_date)
            AND (s.end_date IS NULL OR query_date <= s.end_date)
            AND (
                EXISTS (
                    SELECT 1
                    FROM tx2_vehicle_journey_days_of_operation vjdo
                    WHERE vjdo.document_id = vj.document_id
                      AND vjdo.vehicle_journey_code = vj.vehicle_journey_code
                      AND query_date BETWEEN vjdo.start_date AND vjdo.end_date
                )
                OR EXISTS (
                    SELECT 1
                    FROM tx2_vehicle_journey_days vjd
                    WHERE vjd.document_id = vj.document_id
                      AND vjd.vehicle_journey_code = vj.vehicle_journey_code
                      AND (
                          vjd.day_rule = derived_day_of_week
                          OR (derived_day_of_week IN ('monday','tuesday','wednesday','thursday','friday') AND vjd.day_rule = 'monday_to_friday')
                          OR (derived_day_of_week IN ('monday','tuesday','wednesday','thursday','friday','saturday') AND vjd.day_rule = 'monday_to_saturday')
                          OR vjd.day_rule = 'monday_to_sunday'
                          OR (derived_day_of_week IN ('saturday','sunday') AND vjd.day_rule = 'weekend')
                      )
                )
            )
            AND NOT EXISTS (
                SELECT 1
                FROM tx2_vehicle_journey_days_of_non_operation vjdno
                WHERE vjdno.document_id = vj.document_id
                  AND vjdno.vehicle_journey_code = vj.vehicle_journey_code
                  AND query_date BETWEEN vjdno.start_date AND vjdno.end_date
            )
      ),
      candidate_departures AS (
          SELECT
              fl.location_id AS from_location_id,
              tl.location_id AS to_location_id,
              tl.name AS to_location_name,
              tl.coordinate AS to_location_coordinate,
              origin_leg.departure,
              destination_leg.arrival,
              origin_leg.notes,
              origin_leg.source_modification_datetime
          FROM journey_legs origin_leg
          INNER JOIN journey_legs destination_leg
              ON destination_leg.document_id = origin_leg.document_id
             AND destination_leg.journey_pattern_id = origin_leg.journey_pattern_id
             AND destination_leg.vehicle_journey_code = origin_leg.vehicle_journey_code
             AND destination_leg.global_sort_order >= origin_leg.global_sort_order
          INNER JOIN service_stop_points fl
              ON fl.stop_point_id = origin_leg.from_stop_point_ref
          INNER JOIN service_stop_points tl
              ON tl.stop_point_id = destination_leg.to_stop_point_ref
          WHERE fl.location_id <> tl.location_id
      )
      SELECT DISTINCT ON (from_location_id, to_location_id, departure, arrival, notes)
          from_location_id,
          to_location_id,
          to_location_name,
          to_location_coordinate,
          departure,
          arrival,
          notes
      FROM candidate_departures
      ORDER BY
          from_location_id,
          to_location_id,
          departure,
          arrival,
          notes,
          source_modification_datetime DESC NULLS LAST
    |]
    (date, serviceID, serviceID, serviceID)

getServicesWithScheduledDeparturesV2 :: Application [Int]
getServicesWithScheduledDeparturesV2 = withConnection $ \connection ->
  fmap unwrapOnly
    <$> query_
      connection
      [sql|
        WITH mapped_services AS (
            SELECT DISTINCT sm.service_id
            FROM tx2_service_mappings sm
            JOIN tx2_services s
              ON s.service_code = sm.service_code
            WHERE s.mode = 'ferry'
        ),
        service_stop_points AS (
            SELECT sl.service_id, s.route, l.stop_point_id
            FROM service_locations sl
            JOIN services s ON s.service_id = sl.service_id
            JOIN locations l ON l.location_id = sl.location_id
            WHERE l.stop_point_id IS NOT NULL
        ),
        heuristic_services AS (
            SELECT DISTINCT sp_from.service_id
            FROM service_stop_points sp_from
            JOIN service_stop_points sp_to
              ON sp_to.service_id = sp_from.service_id
             AND sp_to.stop_point_id <> sp_from.stop_point_id
            JOIN tx2_journey_pattern_timing_links jptl
              ON jptl.from_stop_point_ref = sp_from.stop_point_id
             AND jptl.to_stop_point_ref = sp_to.stop_point_id
            JOIN tx2_journey_pattern_sections jps
              ON jps.document_id = jptl.document_id
             AND jps.section_ref = jptl.journey_pattern_section_ref
            JOIN tx2_journey_patterns jp
              ON jp.document_id = jps.document_id
             AND jp.journey_pattern_id = jps.journey_pattern_id
            JOIN tx2_services s
              ON s.document_id = jp.document_id
             AND s.service_code = jp.service_code
            WHERE s.mode = 'ferry'
              AND lower(sp_from.route) NOT LIKE '%freight%'
        )
        SELECT service_id
        FROM mapped_services
        UNION
        SELECT hs.service_id
        FROM heuristic_services hs
        WHERE NOT EXISTS (
            SELECT 1
            FROM tx2_service_mappings sm
            WHERE sm.service_id = hs.service_id
        )
      |]
  where
    unwrapOnly :: Only Int -> Int
    unwrapOnly (Only value) = value

getServiceOrganisations :: Application [ServiceOrganisation]
getServiceOrganisations = withConnection $ \connection ->
  query_
    connection
    [sql|
      SELECT s.service_id, o.organisation_id, o.name, o.website, o.local_phone, o.international_phone, o.email, o.x, o.facebook
      FROM services s
      INNER JOIN organisations o ON s.organisation_id = o.organisation_id
    |]

timeStringToTime :: String -> TimeOfDay
timeStringToTime string =
  let timeParts = splitOn ':' string
   in case timeParts of
        [h, m, s] -> TimeOfDay (read h) (read m) (read s)
        [h, m] -> TimeOfDay (read h) (read m) 0
        _ -> error $ "Unable to convert '" <> string <> "' to TimeOfDay"
