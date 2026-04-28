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
    getServiceHasScheduledDeparturesV2,
    getServicesWithScheduledDeparturesV2,
    getServiceOrganisations,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Pool (Pool, withResource)
import Data.Time.Calendar
  ( Day,
    addDays,
    fromGregorian,
    toGregorian,
  )
import Data.Time.Calendar.WeekDate (toWeekDate)
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
            Just trainServices -> mapMaybe (convertToRailDeparture departureCRS departureName locationID) $ filter (isValidService departureCRS) trainServices
            Nothing -> []

    convertToRailDeparture :: String -> String -> Int -> RailDepartureFetcherTrainService -> Maybe (String, String, String, String, TimeOfDay, String, Bool, Maybe String, Int)
    convertToRailDeparture departureCRS departureName locationID trainService = do
      destination <- listToMaybe $ railDepartureFetcherTrainServiceDestination trainService
      pure
        ( departureCRS,
          departureName,
          railDepartureFetcherLocationCrs destination,
          railDepartureFetcherLocationLocationName destination,
          timeStringToTime . railDepartureFetcherTrainServiceStd $ trainService,
          railDepartureFetcherTrainServiceEtd trainService,
          railDepartureFetcherTrainServiceIsCancelled trainService,
          railDepartureFetcherTrainServicePlatform trainService,
          locationID
        )

    isValidService :: String -> RailDepartureFetcherTrainService -> Bool
    isValidService departureCRS trainService =
      let currentDestination = railDepartureFetcherTrainServiceCurrentDestinations trainService >>= fmap railDepartureFetcherLocationCrs . listToMaybe
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
  let matchedWeekOfMonthRules = matchedWeekOfMonthRulesForDate date
      matchedBankHolidayRules = matchedBankHolidayRulesForDate date
      isBankHoliday = not (null matchedBankHolidayRules)
      matchedWeekOfMonthRulesParam = if null matchedWeekOfMonthRules then ["__no_matching_week_of_month__"] else matchedWeekOfMonthRules
      matchedBankHolidayRulesParam = if null matchedBankHolidayRules then ["__no_matching_bank_holiday__"] else matchedBankHolidayRules
   in query
    connection
    [sql|
      WITH constants(query_date) AS (
          VALUES (date ?)
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
      effective_services AS (
          SELECT
              s.document_id,
              s.service_code
          FROM tx2_services s
          JOIN effective_service_codes esc
            ON esc.service_code = s.service_code
          CROSS JOIN constants
          WHERE s.mode = 'ferry'
            AND (s.start_date IS NULL OR query_date >= s.start_date)
            AND (s.end_date IS NULL OR query_date <= s.end_date)
      ),
      relevant_vehicle_journeys AS (
          SELECT vj.*
          FROM tx2_vehicle_journeys vj
          JOIN effective_services es
            ON es.document_id = vj.document_id
           AND es.service_code = vj.service_code
      ),
      relevant_journey_patterns AS (
          SELECT DISTINCT
              vj.document_id,
              vj.journey_pattern_id
          FROM relevant_vehicle_journeys vj
      ),
      timings AS (
          SELECT
            document_id,
            journey_pattern_timing_link_id,
            journey_pattern_section_ref,
            sort_order,
            from_stop_point_ref,
            from_activity,
            from_timing_status,
            to_stop_point_ref,
            to_activity,
            to_timing_status,
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
          WHERE document_id IN (
              SELECT DISTINCT document_id
              FROM effective_services
          )
      ),
      pattern_links AS (
          SELECT
              jps.document_id,
              jps.journey_pattern_id,
              jps.section_order,
              t.sort_order,
              ((jps.section_order - 1) * 1000) + t.sort_order AS global_sort_order,
              jptl.from_stop_point_ref,
              t.from_activity,
              t.from_timing_status,
              jptl.to_stop_point_ref,
              t.to_activity,
              t.to_timing_status,
              t.wait_time,
              t.run_time
          FROM tx2_journey_pattern_sections jps
          JOIN relevant_journey_patterns rjp
            ON rjp.document_id = jps.document_id
           AND rjp.journey_pattern_id = jps.journey_pattern_id
          JOIN tx2_journey_pattern_timing_links jptl
            ON jptl.document_id = jps.document_id
           AND jptl.journey_pattern_section_ref = jps.section_ref
          JOIN timings t
            ON t.document_id = jptl.document_id
           AND t.journey_pattern_timing_link_id = jptl.journey_pattern_timing_link_id
      ),
      vehicle_journey_links AS (
          SELECT
              vjtl.document_id,
              vjtl.vehicle_journey_code,
              vj.journey_pattern_id,
              vjtl.sort_order AS global_sort_order,
              t.from_stop_point_ref,
              t.from_activity,
              t.from_timing_status,
              t.to_stop_point_ref,
              t.to_activity,
              t.to_timing_status,
              t.wait_time,
              t.run_time
          FROM tx2_vehicle_journey_timing_links vjtl
          JOIN relevant_vehicle_journeys vj
            ON vj.document_id = vjtl.document_id
           AND vj.vehicle_journey_code = vjtl.vehicle_journey_code
          JOIN timings t
            ON t.document_id = vjtl.document_id
           AND t.journey_pattern_timing_link_id = vjtl.journey_pattern_timing_link_id
      ),
      effective_journey_links AS (
          SELECT
              vjl.document_id,
              vjl.vehicle_journey_code,
              vjl.journey_pattern_id,
              vjl.global_sort_order,
              vjl.from_stop_point_ref,
              vjl.from_activity,
              vjl.from_timing_status,
              vjl.to_stop_point_ref,
              vjl.to_activity,
              vjl.to_timing_status,
              vjl.wait_time,
              vjl.run_time
          FROM vehicle_journey_links vjl
          UNION ALL
          SELECT
              vj.document_id,
              vj.vehicle_journey_code,
              pl.journey_pattern_id,
              pl.global_sort_order,
              pl.from_stop_point_ref,
              pl.from_activity,
              pl.from_timing_status,
              pl.to_stop_point_ref,
              pl.to_activity,
              pl.to_timing_status,
              pl.wait_time,
              pl.run_time
          FROM relevant_vehicle_journeys vj
          JOIN pattern_links pl
            ON pl.document_id = vj.document_id
           AND pl.journey_pattern_id = vj.journey_pattern_id
          WHERE NOT EXISTS (
              SELECT 1
              FROM tx2_vehicle_journey_timing_links vjtl
              WHERE vjtl.document_id = vj.document_id
                AND vjtl.vehicle_journey_code = vj.vehicle_journey_code
          )
      ),
      multi_journey_time AS (
          SELECT
              ejl.document_id,
              ejl.vehicle_journey_code,
              ejl.global_sort_order,
              LAG(ejl.run_time) OVER (
                  PARTITION BY ejl.document_id, ejl.vehicle_journey_code
                  ORDER BY ejl.global_sort_order
              ) + ejl.wait_time AS time
          FROM effective_journey_links ejl
      ),
      journey_legs AS (
          SELECT
              vj.document_id,
              vj.journey_pattern_id,
              vj.vehicle_journey_code,
              ejl.global_sort_order,
              ejl.from_stop_point_ref,
              ejl.from_activity,
              ejl.to_stop_point_ref,
              ejl.to_activity,
              (
                  (
                      query_date +
                      COALESCE(
                          SUM(mjt.time) OVER (
                              PARTITION BY vj.document_id, vj.vehicle_journey_code
                              ORDER BY ejl.global_sort_order
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
                              PARTITION BY vj.document_id, vj.vehicle_journey_code
                              ORDER BY ejl.global_sort_order
                          ) + vj.departure_time,
                          vj.departure_time
                      ) + ejl.run_time
                  ) AT TIME ZONE 'Europe/London' AT TIME ZONE 'UTC'
              ) :: TIMESTAMP AS arrival,
              NULLIF(vj.note, '') AS notes,
              d.source_modification_datetime
          FROM relevant_vehicle_journeys vj
          CROSS JOIN constants
          CROSS JOIN day_of_week
          INNER JOIN effective_journey_links ejl
              ON ejl.document_id = vj.document_id
             AND ejl.vehicle_journey_code = vj.vehicle_journey_code
          INNER JOIN multi_journey_time mjt
              ON mjt.document_id = ejl.document_id
             AND mjt.vehicle_journey_code = ejl.vehicle_journey_code
             AND mjt.global_sort_order = ejl.global_sort_order
          INNER JOIN tx2_documents d
              ON d.document_id = vj.document_id
          WHERE ejl.from_activity IN ('', 'pickUp', 'pickUpAndSetDown')
            AND ejl.to_activity IN ('', 'setDown', 'pickUpAndSetDown')
            AND (
                NOT EXISTS (
                    SELECT 1
                    FROM tx2_vehicle_journey_week_of_month_rules vjwmr
                    WHERE vjwmr.document_id = vj.document_id
                      AND vjwmr.vehicle_journey_code = vj.vehicle_journey_code
                )
                OR EXISTS (
                    SELECT 1
                    FROM tx2_vehicle_journey_week_of_month_rules vjwmr
                    WHERE vjwmr.document_id = vj.document_id
                      AND vjwmr.vehicle_journey_code = vj.vehicle_journey_code
                      AND vjwmr.week_of_month_rule IN ?
                )
            )
            AND (
                NOT EXISTS (
                    SELECT 1
                    FROM tx2_vehicle_journey_serviced_organisation_days_of_operation vjsodo
                    WHERE vjsodo.document_id = vj.document_id
                      AND vjsodo.vehicle_journey_code = vj.vehicle_journey_code
                )
                OR EXISTS (
                    SELECT 1
                    FROM tx2_vehicle_journey_serviced_organisation_days_of_operation vjsodo
                    WHERE vjsodo.document_id = vj.document_id
                      AND vjsodo.vehicle_journey_code = vj.vehicle_journey_code
                      AND query_date BETWEEN vjsodo.start_date AND vjsodo.end_date
                )
            )
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
                          (vjd.day_rule = 'holidays_only' AND EXISTS (
                              SELECT 1
                              WHERE ?
                          ))
                          OR
                          vjd.day_rule = derived_day_of_week
                          OR (derived_day_of_week IN ('monday','tuesday','wednesday','thursday','friday') AND vjd.day_rule = 'monday_to_friday')
                          OR (derived_day_of_week IN ('monday','tuesday','wednesday','thursday','friday','saturday') AND vjd.day_rule = 'monday_to_saturday')
                          OR vjd.day_rule = 'monday_to_sunday'
                          OR (derived_day_of_week IN ('saturday','sunday') AND vjd.day_rule = 'weekend')
                      )
                )
                OR EXISTS (
                    SELECT 1
                    FROM tx2_vehicle_journey_bank_holiday_operation_rules vjbhor
                    WHERE vjbhor.document_id = vj.document_id
                      AND vjbhor.vehicle_journey_code = vj.vehicle_journey_code
                      AND vjbhor.bank_holiday_rule IN ?
                )
            )
            AND NOT EXISTS (
                SELECT 1
                FROM tx2_vehicle_journey_serviced_organisation_days_of_non_operation vjsodno
                WHERE vjsodno.document_id = vj.document_id
                  AND vjsodno.vehicle_journey_code = vj.vehicle_journey_code
                  AND query_date BETWEEN vjsodno.start_date AND vjsodno.end_date
            )
            AND NOT EXISTS (
                SELECT 1
                FROM tx2_vehicle_journey_days_of_non_operation vjdno
                WHERE vjdno.document_id = vj.document_id
                  AND vjdno.vehicle_journey_code = vj.vehicle_journey_code
                  AND query_date BETWEEN vjdno.start_date AND vjdno.end_date
            )
            AND NOT EXISTS (
                SELECT 1
                FROM tx2_vehicle_journey_bank_holiday_non_operation_rules vjbhnor
                WHERE vjbhnor.document_id = vj.document_id
                  AND vjbhnor.vehicle_journey_code = vj.vehicle_journey_code
                  AND vjbhnor.bank_holiday_rule IN ?
            )
      ),
      candidate_departures AS (
          SELECT
              fl.location_id AS from_location_id,
              tl.location_id AS to_location_id,
              tl.name AS to_location_name,
              tl.coordinate AS to_location_coordinate,
              jl.departure,
              jl.arrival,
              jl.notes,
              jl.source_modification_datetime
          FROM journey_legs jl
          INNER JOIN service_stop_points fl
              ON fl.stop_point_id = jl.from_stop_point_ref
          INNER JOIN service_stop_points tl
              ON tl.stop_point_id = jl.to_stop_point_ref
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
    (date, serviceID, serviceID, serviceID, In matchedWeekOfMonthRulesParam, isBankHoliday, In matchedBankHolidayRulesParam, In matchedBankHolidayRulesParam)

getServiceHasScheduledDeparturesV2 :: Int -> Application Bool
getServiceHasScheduledDeparturesV2 serviceID = withConnection $ \connection -> do
  results <-
    query
      connection
      [sql|
        WITH selected_service AS (
            SELECT route
            FROM services
            WHERE service_id = ?
        ),
        service_stop_points AS (
            SELECT l.stop_point_id
            FROM service_locations sl
            JOIN locations l ON l.location_id = sl.location_id
            WHERE sl.service_id = ?
              AND l.stop_point_id IS NOT NULL
        ),
        mapped_service_codes AS (
            SELECT 1
            FROM tx2_service_mappings sm
            JOIN tx2_services s
              ON s.service_code = sm.service_code
            WHERE sm.service_id = ?
              AND s.mode = 'ferry'
            LIMIT 1
        ),
        heuristic_service_codes AS (
            SELECT 1
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
            LIMIT 1
        )
        SELECT
          EXISTS (SELECT 1 FROM mapped_service_codes)
          OR (
            NOT EXISTS (
              SELECT 1
              FROM tx2_service_mappings sm
              WHERE sm.service_id = ?
            )
            AND EXISTS (SELECT 1 FROM heuristic_service_codes)
          )
      |]
      (serviceID, serviceID, serviceID, serviceID)
  pure $ maybe False (\(Only value) -> value) (listToMaybe results)

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

matchedBankHolidayRulesForDate :: Day -> [String]
matchedBankHolidayRulesForDate day =
  nub $
    ["all_bank_holidays" | isAnyScottishBankHoliday day]
      <> ["other_public_holiday" | isAnyScottishBankHoliday day]
      <> [ruleName | (ruleName, ruleDay) <- specificScottishBankHolidays year, day == ruleDay]
      <> ["displacement_holidays" | isDisplacementHoliday day]
      <> ["holiday_mondays" | isMonday day && isAnyScottishBankHoliday day]
      <> ["all_holidays_except_christmas" | isAnyScottishBankHoliday day && not (isChristmasRelatedHoliday day)]
      <> ["no_holidays" | not (isAnyScottishBankHoliday day)]
      <> ["christmas_eve" | isChristmasEve day]
      <> ["new_years_eve" | isNewYearsEve day]
      <> ["early_run_off_days" | isEarlyRunOffDay day]
  where
    (year, _, _) = toGregorian day

matchedWeekOfMonthRulesForDate :: Day -> [String]
matchedWeekOfMonthRulesForDate day =
  ["every_week", ordinalWeekOfMonthRule day]
    <> ["last" | isLastWeekdayOfMonth day]

ordinalWeekOfMonthRule :: Day -> String
ordinalWeekOfMonthRule day =
  case ((dayOfMonth - 1) `div` 7) + 1 of
    1 -> "first"
    2 -> "second"
    3 -> "third"
    4 -> "fourth"
    _ -> "fifth"
  where
    (_, _, dayOfMonth) = toGregorian day

isLastWeekdayOfMonth :: Day -> Bool
isLastWeekdayOfMonth day =
  let (year, month, _) = toGregorian day
      (nextYear, nextMonth, _) = toGregorian (addDays 7 day)
   in year /= nextYear || month /= nextMonth

specificScottishBankHolidays :: Integer -> [(String, Day)]
specificScottishBankHolidays year =
  [ ("new_years_day", fromGregorian year 1 1),
    ("new_years_day_holiday", observedNewYearsDay year),
    ("jan2nd_scotland", observedJan2ndScotland year),
    ("good_friday", addDays (-2) easterSunday),
    ("easter_monday", addDays 1 easterSunday),
    ("may_day", firstMondayOfMonth year 5),
    ("spring_bank", lastMondayOfMonth year 5),
    ("august_bank_holiday_scotland", firstMondayOfMonth year 8),
    ("late_summer_bank_holiday_not_scotland", lastMondayOfMonth year 8),
    ("st_andrews_day", observedStAndrewsDay year),
    ("christmas_day", fromGregorian year 12 25),
    ("christmas_day_holiday", observedChristmasDay year),
    ("boxing_day", fromGregorian year 12 26),
    ("boxing_day_holiday", observedBoxingDay year)
  ]
  where
    easterSunday = gregorianEasterSunday year

isAnyScottishBankHoliday :: Day -> Bool
isAnyScottishBankHoliday day =
  day `elem` fmap snd (specificScottishBankHolidays year)
  where
    (year, _, _) = toGregorian day

isDisplacementHoliday :: Day -> Bool
isDisplacementHoliday day =
  day
    `elem` filter (/= baseHoliday) observedHolidays
  where
    (year, _, _) = toGregorian day
    observedHolidays =
      [ observedNewYearsDay year,
        observedJan2ndScotland year,
        observedStAndrewsDay year,
        observedChristmasDay year,
        observedBoxingDay year
      ]
    baseHoliday =
      case day of
        candidate
          | candidate == observedNewYearsDay year -> fromGregorian year 1 1
          | candidate == observedJan2ndScotland year -> fromGregorian year 1 2
          | candidate == observedStAndrewsDay year -> fromGregorian year 11 30
          | candidate == observedChristmasDay year -> fromGregorian year 12 25
          | candidate == observedBoxingDay year -> fromGregorian year 12 26
          | otherwise -> candidate

isChristmasRelatedHoliday :: Day -> Bool
isChristmasRelatedHoliday day =
  let (year, _, _) = toGregorian day
   in day == fromGregorian year 12 25
        || day == observedChristmasDay year

isChristmasEve :: Day -> Bool
isChristmasEve day =
  let (_, month, dayOfMonth) = toGregorian day
   in month == 12 && dayOfMonth == 24

isNewYearsEve :: Day -> Bool
isNewYearsEve day =
  let (_, month, dayOfMonth) = toGregorian day
   in month == 12 && dayOfMonth == 31

isEarlyRunOffDay :: Day -> Bool
isEarlyRunOffDay day =
  isChristmasEve day || isNewYearsEve day

firstMondayOfMonth :: Integer -> Int -> Day
firstMondayOfMonth year month =
  case [candidate | dayOfMonth <- [1 .. 7], let candidate = fromGregorian year month dayOfMonth, isMonday candidate] of
    firstMonday : _ -> firstMonday
    [] -> fromGregorian year month 1

lastMondayOfMonth :: Integer -> Int -> Day
lastMondayOfMonth year month =
  case [candidate | offset <- [0 .. 6], let candidate = addDays (negate offset) monthEnd, isMonday candidate] of
    lastMonday : _ -> lastMonday
    [] -> monthEnd
  where
    monthEnd =
      addDays
        (-1)
        ( if month == 12
            then fromGregorian (year + 1) 1 1
            else fromGregorian year (month + 1) 1
        )

observedNewYearsDay :: Integer -> Day
observedNewYearsDay year =
  case weekday (fromGregorian year 1 1) of
    6 -> fromGregorian year 1 3
    7 -> fromGregorian year 1 2
    _ -> fromGregorian year 1 1

observedJan2ndScotland :: Integer -> Day
observedJan2ndScotland year =
  case weekday (fromGregorian year 1 2) of
    6 -> fromGregorian year 1 4
    7 -> fromGregorian year 1 3
    _ -> fromGregorian year 1 2

observedChristmasDay :: Integer -> Day
observedChristmasDay year =
  case weekday (fromGregorian year 12 25) of
    6 -> fromGregorian year 12 27
    7 -> fromGregorian year 12 27
    _ -> fromGregorian year 12 25

observedBoxingDay :: Integer -> Day
observedBoxingDay year =
  case weekday (fromGregorian year 12 26) of
    6 -> fromGregorian year 12 28
    7 -> fromGregorian year 12 28
    _ ->
      if observedChristmasDay year == fromGregorian year 12 26
        then fromGregorian year 12 27
        else fromGregorian year 12 26

observedStAndrewsDay :: Integer -> Day
observedStAndrewsDay year =
  case weekday (fromGregorian year 11 30) of
    6 -> fromGregorian year 12 2
    7 -> fromGregorian year 12 1
    _ -> fromGregorian year 11 30

weekday :: Day -> Int
weekday day =
  let (_, _, weekDay) = toWeekDate day
   in weekDay

isMonday :: Day -> Bool
isMonday day = weekday day == 1

-- Calculates Easter Sunday using the anonymous Gregorian computus.
-- The arithmetic comes from the standard Meeus/Jones/Butcher-style algorithm:
-- it derives the Paschal full moon and then the following Sunday from the year
-- number alone, which is why the intermediate values look opaque but are all
-- calendar terms rather than business-specific logic.
gregorianEasterSunday :: Integer -> Day
gregorianEasterSunday year = fromGregorian year month day
  where
    a = year `mod` 19
    b = year `div` 100
    c = year `mod` 100
    d = b `div` 4
    e = b `mod` 4
    f = (b + 8) `div` 25
    g = (b - f + 1) `div` 3
    h = (19 * a + b - d - g + 15) `mod` 30
    i = c `div` 4
    k = c `mod` 4
    l = (32 + 2 * e + 2 * i - h - k) `mod` 7
    m = (a + 11 * h + 22 * l) `div` 451
    month = fromInteger ((h + l - 7 * m + 114) `div` 31)
    day = fromInteger (((h + l - 7 * m + 114) `mod` 31) + 1)
