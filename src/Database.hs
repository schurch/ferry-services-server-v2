{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database
  ( insertLocationWeather,
    getLocationWeathers,
    getService,
    getServices,
    getServicesForOrganisation,
    hideServicesWithIDs,
    createInstallation,
    addServiceToInstallation,
    deleteServiceForInstallation,
    getServicesForInstallation,
    getInstallationWithID,
    getIntererestedInstallationsForServiceID,
    saveServices,
    deleteInstallationWithID,
    getServiceLocations,
    getLocations,
    saveVessel,
    getVessels,
    getServiceVessels,
    getLocationDepartures,
    getServiceOrganisations,
    updateTransxchangeData,
  )
where

import Control.Monad (forM_, void)
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
import Database.PostgreSQL.Simple.Types (PGArray (..))
import System.Environment (getEnv)
import TransxchangeTypes
import Types
import Utility (splitOn)

withConnection :: (Connection -> IO a) -> Application a
withConnection action = do
  connectionPool <- asks connectionPool
  liftIO $ withResource connectionPool action

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

getInstallationWithID :: UUID -> Application (Maybe Installation)
getInstallationWithID installationID = do
  results <- withConnection $ \connection ->
    query
      connection
      [sql|
      SELECT i.installation_id, i.device_token, i.device_type, i.endpoint_arn, i.updated
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

getLocationDepartures :: Int -> Day -> Application [LocationDeparture]
getLocationDepartures serviceID date = withConnection $ \connection ->
  query
    connection
    [sql|
      WITH constants(query_date) AS (
          VALUES (date ?)
      ),
      timings AS (
          SELECT 
              journey_pattern_timing_link_id,
              COALESCE(NULLIF(regexp_replace(from_wait_time, '\D','','g'), '') :: interval, '0 seconds') AS wait_time,
              COALESCE(NULLIF(regexp_replace(run_time, '\D','','g'), '') :: interval, '0 seconds') AS run_time
          FROM
              journey_pattern_timing_links
      ),
      day_of_week(derived_day_of_week) AS (
          SELECT 
              CASE 
                  WHEN extract(dow from query_date) = 0 THEN 'sunday' :: day_of_week
                  WHEN extract(dow from query_date) = 1 THEN 'monday' :: day_of_week
                  WHEN extract(dow from query_date) = 2 THEN 'tuesday' :: day_of_week
                  WHEN extract(dow from query_date) = 3 THEN 'wednesday' :: day_of_week
                  WHEN extract(dow from query_date) = 4 THEN 'thursday' :: day_of_week
                  WHEN extract(dow from query_date) = 5 THEN 'friday' :: day_of_week
                  WHEN extract(dow from query_date) = 6 THEN 'saturday' :: day_of_week
              END
          FROM constants
      ),
      operating_today AS (
          SELECT 
              vehicle_journey_code
          FROM 
              days_of_operation, constants
          WHERE 
              query_date >= start_date
              AND
              query_date <= end_date

      ),
      not_operating_today AS (
          SELECT 
              vehicle_journey_code
          FROM 
              days_of_non_operation, constants
          WHERE 
              query_date >= start_date
              AND
              query_date <= end_date
      ),
      serviced_org_not_operating_today AS (
          SELECT 
              vehicle_journey_code
          FROM 
              vehicle_journeys vj
          CROSS JOIN
              constants
          INNER JOIN
              serviced_organisation_working_days sowd ON sowd.serviced_organisation_code = vj.non_operation_serviced_organisation_code
          WHERE 
              query_date >= start_date
              AND
              query_date <= end_date
      ),
      multi_journey_time AS (
          SELECT 
              jptl.journey_pattern_timing_link_id,
              LAG(t.run_time) OVER (
                  PARTITION BY jptl.journey_pattern_section_id 
                  ORDER BY jptl.journey_pattern_timing_link_id
              ) + t.wait_time AS time
          FROM
              journey_pattern_timing_links jptl
          INNER JOIN
              timings t ON t.journey_pattern_timing_link_id = jptl.journey_pattern_timing_link_id
      )
      SELECT
          fl.location_id AS from_location_id, 
          tl.location_id AS to_location_id, 
          tl.name AS to_location_name, 
          tl.coordinate AS to_location_coordinate, 
          (
              (
                  query_date +
                  COALESCE (
                      SUM (mjt.time) OVER (
                          PARTITION BY jptl.journey_pattern_section_id, vj.vehicle_journey_code
                          ORDER BY jptl.journey_pattern_timing_link_id
                      ) + vj.departure_time,
                      departure_time
                  )
              ) AT TIME ZONE 'Europe/London' AT TIME ZONE 'UTC'
          ) :: TIMESTAMP AS departure,
          (
              (
                  query_date +
                  COALESCE (
                      SUM (mjt.time) OVER (
                          PARTITION BY jptl.journey_pattern_section_id, vj.vehicle_journey_code
                          ORDER BY jptl.journey_pattern_timing_link_id
                      ) + vj.departure_time,
                      departure_time
                  ) + t.run_time 
              ) AT TIME ZONE 'Europe/London' AT TIME ZONE 'UTC'
          ) :: TIMESTAMP AS arrival,
          NULLIF(vj.note, '')
      FROM 
          vehicle_journeys vj
      CROSS JOIN
          constants
      CROSS JOIN
          day_of_week
      INNER JOIN 
          journey_patterns jp ON vj.journey_pattern_id = jp.journey_pattern_id
      INNER JOIN 
          journey_pattern_timing_links jptl ON jp.journey_pattern_section_id = jptl.journey_pattern_section_id
      INNER JOIN
          timings t ON t.journey_pattern_timing_link_id = jptl.journey_pattern_timing_link_id
      INNER JOIN 
          transxchange_services txcs ON txcs.service_code = vj.service_code
      INNER JOIN 
          services s ON s.service_id = txcs.service_id
      INNER JOIN
          locations fl ON fl.stop_point_id = jptl.from_stop_point
      INNER JOIN
          locations tl ON tl.stop_point_id = jptl.to_stop_point
      INNER JOIN
          multi_journey_time mjt ON mjt.journey_pattern_timing_link_id = jptl.journey_pattern_timing_link_id
      WHERE 
          s.service_id = ?
          AND
          (
              query_date >= txcs.start_date AND query_date <= txcs.end_date
              OR
              vj.vehicle_journey_code IN (SELECT * FROM operating_today)
          )
          AND
          derived_day_of_week = ANY (vj.days_of_week)
          AND
          vj.vehicle_journey_code NOT IN (SELECT * FROM not_operating_today)
          AND
          vj.vehicle_journey_code NOT IN (SELECT * FROM serviced_org_not_operating_today)
      ORDER BY
          fl.name,
          departure
    |]
    (date, serviceID)

getServiceOrganisations :: Application [ServiceOrganisation]
getServiceOrganisations = withConnection $ \connection ->
  query_
    connection
    [sql|
      SELECT s.service_id, o.organisation_id, o.name, o.website, o.local_phone, o.international_phone, o.email, o.x, o.facebook
      FROM services s
      INNER JOIN organisations o ON s.organisation_id = o.organisation_id
    |]

updateTransxchangeData :: [TransXChangeData] -> Application ()
updateTransxchangeData transxchangeData = withConnection $ \connection ->
  withTransaction connection $ do
    deleteOldData connection
    forM_ transxchangeData $ updateSingleTransxchangeData connection
  where
    deleteOldData :: Connection -> IO ()
    deleteOldData connection = do
      void $
        execute_
          connection
          [sql| 
        DELETE FROM days_of_non_operation;
        DELETE FROM days_of_operation;
        DELETE FROM serviced_organisation_working_days;
        DELETE FROM vehicle_journeys;
        DELETE FROM serviced_organisation_working_days;
        DELETE FROM serviced_organisations;
        DELETE FROM lines;
        DELETE FROM journey_patterns;
        DELETE FROM journey_pattern_timing_links;
        DELETE FROM journey_pattern_sections;
        DELETE FROM routes;
        DELETE FROM route_links;
        DELETE FROM route_sections;
        DELETE FROM stop_points;
      |]

updateSingleTransxchangeData :: Connection -> TransXChangeData -> IO ()
updateSingleTransxchangeData connection (TransXChangeData stopPoints servicedOrganisations routeSections routes journeyPatternSections operators services vehicleJourneys) =
  do
    insertStopPoints connection stopPoints
    insertServicedOrganisations connection servicedOrganisations
    insertRouteSections connection routeSections
    insertRoutes connection routes
    insertJourneyPatternSections connection journeyPatternSections
    insertOperators connection operators
    insertServices connection services
    insertVehicleJourneys connection vehicleJourneys
  where
    insertStopPoints :: Connection -> [AnnotatedStopPointRef] -> IO ()
    insertStopPoints connection stopPoints = do
      let statement =
            [sql| 
                      INSERT INTO stop_points (stop_point_id, common_name) 
                      VALUES (?,?)
                      ON CONFLICT (stop_point_id) DO UPDATE 
                        SET common_name = excluded.common_name
                    |]
      let values =
            (\(AnnotatedStopPointRef id commonName) -> (id, commonName))
              <$> stopPoints
      void $ executeMany connection statement values

    insertServicedOrganisations :: Connection -> [ServicedOrganisation] -> IO ()
    insertServicedOrganisations connection servicedOrganisations = do
      let statement =
            [sql| 
                      INSERT INTO serviced_organisations (serviced_organisation_code, name) 
                      VALUES (?,?)
                      ON CONFLICT (serviced_organisation_code) DO UPDATE 
                        SET name = excluded.name
                    |]
      let values =
            (\(ServicedOrganisation code name _) -> (code, name))
              <$> servicedOrganisations
      void $ executeMany connection statement values
      forM_ servicedOrganisations $ \(ServicedOrganisation code _ workingDays) ->
        insertServicedOrganisationWorkingDays connection code workingDays

    insertServicedOrganisationWorkingDays ::
      Connection -> String -> [DateRange] -> IO ()
    insertServicedOrganisationWorkingDays connection servicedOrganisationCode workingDays =
      do
        let statement =
              [sql| 
                      INSERT INTO serviced_organisation_working_days (serviced_organisation_code, start_date, end_date) 
                      VALUES (?, ?, ?)
                      ON CONFLICT (serviced_organisation_code, start_date, end_date) DO NOTHING
                    |]
        let values =
              (\(DateRange start end) -> (servicedOrganisationCode, start, end))
                <$> workingDays
        void $ executeMany connection statement values

    insertRouteSections :: Connection -> [RouteSection] -> IO ()
    insertRouteSections connection routeSections = do
      let statement =
            [sql| 
                      INSERT INTO route_sections (route_section_id) 
                      VALUES (?)
                      ON CONFLICT (route_section_id) DO NOTHING
                    |]
      let values =
            (\(RouteSection routeSectionID _) -> Only routeSectionID)
              <$> routeSections
      void $ executeMany connection statement values
      forM_ routeSections $ \(RouteSection routeSectionID links) ->
        insertRouteLinks connection routeSectionID links

    insertRouteLinks :: Connection -> String -> [RouteLink] -> IO ()
    insertRouteLinks connection routeSectionID routeLinks = do
      let statement =
            [sql| 
                      INSERT INTO route_links (route_link_id, route_section_id, from_stop_point, to_stop_point, route_direction) 
                      VALUES (?, ?, ?, ?, ?)
                      ON CONFLICT (route_link_id) DO UPDATE 
                        SET route_section_id = excluded.route_section_id,
                            from_stop_point = excluded.from_stop_point,
                            to_stop_point = excluded.to_stop_point, 
                            route_direction = excluded.route_direction
                    |]
      let values =
            ( \(RouteLink routeLinkID fromStopPoint toStopPoint direction) ->
                (routeLinkID, routeSectionID, fromStopPoint, toStopPoint, direction)
            )
              <$> routeLinks
      void $ executeMany connection statement values

    insertRoutes :: Connection -> [Route] -> IO ()
    insertRoutes connection routes = do
      let statement =
            [sql| 
                      INSERT INTO routes (route_id, route_description, route_section_id) 
                      VALUES (?, ?, ?)
                      ON CONFLICT (route_id) DO UPDATE 
                        SET route_description = excluded.route_description,
                            route_section_id = excluded.route_section_id
                    |]
      let values =
            ( \(Route routeID routeDescription routeSectionID) ->
                (routeID, routeDescription, routeSectionID)
            )
              <$> routes
      void $ executeMany connection statement values

    insertJourneyPatternSections ::
      Connection -> [JourneyPatternSection] -> IO ()
    insertJourneyPatternSections connection journeyPatternSections = do
      let statement =
            [sql| 
                      INSERT INTO journey_pattern_sections (journey_pattern_section_id) 
                      VALUES (?)
                      ON CONFLICT (journey_pattern_section_id) DO NOTHING
                    |]
      let values =
            ( \(JourneyPatternSection journeyPatterSectionID _) ->
                Only journeyPatterSectionID
            )
              <$> journeyPatternSections
      void $ executeMany connection statement values
      forM_ journeyPatternSections $
        \(JourneyPatternSection journeyPatterSectionID timingLinks) ->
          insertJourneyPatternTimingLinks
            connection
            journeyPatterSectionID
            timingLinks

    insertJourneyPatternTimingLinks ::
      Connection -> String -> [JourneyPatternTimingLink] -> IO ()
    insertJourneyPatternTimingLinks connection journeyPatterSectionID timingLinks =
      do
        let statement =
              [sql| 
                        INSERT INTO journey_pattern_timing_links (
                          journey_pattern_timing_link_id, 
                          journey_pattern_section_id, 
                          from_stop_point, 
                          from_timing_status, 
                          from_wait_time, 
                          to_stop_point, 
                          to_timing_status, 
                          route_link_id, 
                          journey_direction, 
                          run_time) 
                        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                        ON CONFLICT (journey_pattern_timing_link_id) DO UPDATE 
                          SET journey_pattern_section_id = excluded.journey_pattern_section_id,
                              from_stop_point = excluded.from_stop_point,
                              from_timing_status = excluded.from_timing_status, 
                              from_wait_time = excluded.from_wait_time, 
                              to_stop_point = excluded.to_stop_point, 
                              to_timing_status = excluded.to_timing_status, 
                              route_link_id = excluded.route_link_id, 
                              journey_direction = excluded.journey_direction, 
                              run_time = excluded.run_time
                    |]
        let values =
              ( \(JourneyPatternTimingLink journeyPatternTimingLinkId journeyPatternFromWaitTime journeyPatternFromStopPointRef journeyPatternFromTimingStatus journeyPatternToStopPointsRef journeyPatternToTimingStatus routeLinkRef journeyDirection runTime) ->
                  ( journeyPatternTimingLinkId,
                    journeyPatterSectionID,
                    journeyPatternFromStopPointRef,
                    journeyPatternFromTimingStatus,
                    journeyPatternFromWaitTime,
                    journeyPatternToStopPointsRef,
                    journeyPatternToTimingStatus,
                    routeLinkRef,
                    journeyDirection,
                    runTime
                  )
              )
                <$> timingLinks
        void $ executeMany connection statement values

    insertOperators :: Connection -> [Operator] -> IO ()
    insertOperators connection operators = do
      let statement =
            [sql| 
                      INSERT INTO operators (operator_id, national_operator_code, operator_code, operator_short_name) 
                      VALUES (?, ?, ?, ?)
                      ON CONFLICT (operator_id) DO UPDATE 
                        SET national_operator_code = excluded.national_operator_code,
                            operator_code = excluded.operator_code,
                            operator_short_name = excluded.operator_short_name
                    |]
      let values =
            ( \(Operator operatorID nationalOperatorCode operatorCode operatorShortName) ->
                (operatorID, nationalOperatorCode, operatorCode, operatorShortName)
            )
              <$> operators
      void $ executeMany connection statement values

    insertServices :: Connection -> [TransxchangeTypes.Service] -> IO ()
    insertServices connection services = do
      let statement =
            [sql| 
                      INSERT INTO transxchange_services (service_code, operator_id, mode, description, start_date, end_date, origin, destination) 
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                      ON CONFLICT (service_code) DO UPDATE 
                        SET operator_id = excluded.operator_id,
                            mode = excluded.mode,
                            description = excluded.description,
                            start_date = excluded.start_date,
                            end_date = excluded.end_date,
                            origin = excluded.origin,
                            destination = excluded.destination
                    |]
      let values =
            ( \(TransxchangeTypes.Service serviceCode lines (DateRange startDate endDate) registeredOperatorRef mode description (StandardService origin destination _)) ->
                ( serviceCode,
                  registeredOperatorRef,
                  mode,
                  description,
                  startDate,
                  endDate,
                  origin,
                  destination
                )
            )
              <$> services
      void $ executeMany connection statement values
      forM_ services $
        \TransxchangeTypes.Service {serviceCode = serviceCode, TransxchangeTypes.lines = lines, standardService = StandardService {journeyPatterns = journeyPatterns}} ->
          do
            insertLines connection serviceCode lines
            insertJourneyPatterns connection serviceCode journeyPatterns

    insertLines :: Connection -> String -> [Line] -> IO ()
    insertLines connection serviceCode lines = do
      let statement =
            [sql| 
                      INSERT INTO lines (line_id, service_code, line_name) 
                      VALUES (?, ?, ?)
                      ON CONFLICT (line_id) DO UPDATE 
                        SET service_code = excluded.service_code,
                            line_name = excluded.line_name
                    |]
      let values =
            (\(Line lineID lineName) -> (lineID, serviceCode, lineName)) <$> lines
      void $ executeMany connection statement values

    insertJourneyPatterns :: Connection -> String -> [JourneyPattern] -> IO ()
    insertJourneyPatterns connection serviceCode journeyPatterns = do
      let statement =
            [sql| 
                      INSERT INTO journey_patterns (journey_pattern_id, service_code, journey_pattern_section_id, direction) 
                      VALUES (?, ?, ?, ?)
                      ON CONFLICT (journey_pattern_id) DO UPDATE 
                        SET service_code = excluded.service_code,
                            journey_pattern_section_id = excluded.journey_pattern_section_id,
                            direction = excluded.direction
                    |]
      let values =
            ( \(JourneyPattern journeyPatternID journeyPatternDirection journeyPatternSectionRef) ->
                ( journeyPatternID,
                  serviceCode,
                  journeyPatternSectionRef,
                  journeyPatternDirection
                )
            )
              <$> journeyPatterns
      void $ executeMany connection statement values

    insertVehicleJourneys :: Connection -> [VehicleJourney] -> IO ()
    insertVehicleJourneys connection vehicleJourneys = do
      let statement =
            [sql| 
                      INSERT INTO vehicle_journeys (vehicle_journey_code, service_code, line_id, journey_pattern_id, operator_id, days_of_week, departure_time, note, note_code, non_operation_serviced_organisation_code) 
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                      ON CONFLICT (vehicle_journey_code) DO UPDATE 
                        SET service_code = excluded.service_code,
                            line_id = excluded.line_id,
                            journey_pattern_id = excluded.journey_pattern_id,
                            operator_id = excluded.operator_id,
                            days_of_week = excluded.days_of_week,
                            departure_time = excluded.departure_time,
                            note = excluded.note,
                            note_code = excluded.note_code,
                            non_operation_serviced_organisation_code = excluded.non_operation_serviced_organisation_code
                    |]
      let values =
            ( \(VehicleJourney operatorRef vehicleJourneyCode serviceRef lineRef journeyPatternRef departureTime daysOfWeek _ _ note noteCode daysOfNonOperationServicedOrganisationRef) ->
                ( vehicleJourneyCode,
                  serviceRef,
                  lineRef,
                  journeyPatternRef,
                  operatorRef,
                  PGArray daysOfWeek,
                  timeStringToTime departureTime,
                  note,
                  noteCode,
                  daysOfNonOperationServicedOrganisationRef
                )
            )
              <$> vehicleJourneys
      void $ executeMany connection statement values
      forM_ vehicleJourneys $
        \VehicleJourney {vehicleJourneyCode = vehicleJourneyCode, specialDaysOfOperation = specialDaysOfOperation, specialDaysOfNonOperation = specialDaysOfNonOperation} ->
          do
            insertDayOfOperation
              connection
              vehicleJourneyCode
              specialDaysOfOperation
            insertDayOfNonOperation
              connection
              vehicleJourneyCode
              specialDaysOfNonOperation

    insertDayOfOperation :: Connection -> String -> [DateRange] -> IO ()
    insertDayOfOperation connection vehicleJourneyCode daysOfOperation = do
      let statement =
            [sql| 
                      INSERT INTO days_of_operation (vehicle_journey_code, start_date, end_date) 
                      VALUES (?, ?, ?)
                      ON CONFLICT (vehicle_journey_code, start_date, end_date) DO NOTHING
                    |]
      let values =
            (\(DateRange start end) -> (vehicleJourneyCode, start, end))
              <$> daysOfOperation
      void $ executeMany connection statement values

    insertDayOfNonOperation :: Connection -> String -> [DateRange] -> IO ()
    insertDayOfNonOperation connection vehicleJourneyCode daysOfOperation = do
      let statement =
            [sql| 
                      INSERT INTO days_of_non_operation (vehicle_journey_code, start_date, end_date) 
                      VALUES (?, ?, ?)
                      ON CONFLICT (vehicle_journey_code, start_date, end_date) DO NOTHING
                    |]
      let values =
            (\(DateRange start end) -> (vehicleJourneyCode, start, end))
              <$> daysOfOperation
      void $ executeMany connection statement values

    timeStringToTime :: String -> TimeOfDay
    timeStringToTime string =
      let timeParts = splitOn ':' string
       in TimeOfDay
            (read $ head timeParts)
            (read $ timeParts !! 1)
            (read $ timeParts !! 2)