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
  , getLocationDeparturesForServiceID
  , updateTransxchangeData
  )
where

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
                                                , LocationDeparture
                                                , Location
                                                )
import           Data.Time.LocalTime            ( TimeOfDay(..) )
import           Utility                        ( splitOn )
import           Data.Time.Calendar             ( Day )
import           TransxchangeTypes

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

getIntererestedInstallationsForServiceID :: MonadIO m => Int -> m [Installation]
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

getLocationDeparturesForServiceID
  :: MonadIO m => Int -> Day -> m [LocationDeparture]
getLocationDeparturesForServiceID serviceID date = do
  withConnection $ \connection -> query
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
      multi_journeys AS (
          SELECT 
              journey_pattern_section_id 
          FROM 
              journey_pattern_timing_links 
          GROUP BY 
              journey_pattern_section_id HAVING COUNT(*) > 1
      )
      SELECT
          fl.location_id AS from_location_id, 
          tl.location_id AS to_location_id, 
          tl.name, 
          tl.latitude, 
          tl.longitude, 
          CASE 
              WHEN jptl.journey_pattern_section_id IN (SELECT * FROM multi_journeys) THEN
                  COALESCE (
                      LAG (departure_time + t.run_time, 1) OVER (
                          PARTITION BY jptl.journey_pattern_section_id
                          ORDER BY jptl.journey_pattern_timing_link_id ASC
                      ) + t.wait_time, 
                      departure_time
                  )
              ELSE departure_time
          END AS departure_time,
          jptl.run_time
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
          services s ON s.transxchange_service_code = txcs.service_code
      INNER JOIN
          locations fl ON fl.stop_point_id = jptl.from_stop_point
      INNER JOIN
          locations tl ON tl.stop_point_id = jptl.to_stop_point
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
    |]
    (date, serviceID)

updateTransxchangeData :: MonadIO m => [TransXChangeData] -> m ()
updateTransxchangeData transxchangeData = withConnection $ \connection ->
  withTransaction connection $ do
    deleteOldData connection
    forM_ transxchangeData $ updateSingleTransxchangeData connection
 where
  deleteOldData :: Connection -> IO ()
  deleteOldData connection = do
    void $ execute_
      connection
      [sql| 
        DELETE FROM days_of_non_operation;
        DELETE FROM days_of_operation;
        DELETE FROM vehicle_journeys;
        DELETE FROM journey_patterns;
        DELETE FROM lines;
        DELETE FROM transxchange_services;
        DELETE FROM operators;
        DELETE FROM journey_pattern_timing_links;
        DELETE FROM journey_pattern_sections;
        DELETE FROM routes;
        DELETE FROM route_links;
        DELETE FROM route_sections;
        DELETE FROM stop_points;
      |]

updateSingleTransxchangeData :: Connection -> TransXChangeData -> IO ()
updateSingleTransxchangeData connection (TransXChangeData stopPoints servicedOrganisations routeSections routes journeyPatternSections operators services vehicleJourneys)
  = do
    insertStopPoints connection stopPoints
    insertRouteSections connection routeSections
    insertRoutes connection routes
    insertJourneyPatternSections connection journeyPatternSections
    insertOperators connection operators
    insertServices connection services
    insertVehicleJourneys connection vehicleJourneys
 where
  insertStopPoints :: Connection -> [AnnotatedStopPointRef] -> IO ()
  insertStopPoints connection stopPoints = do
    let statement = [sql| 
                      INSERT INTO stop_points (stop_point_id, common_name) 
                      VALUES (?,?)
                      ON CONFLICT (stop_point_id) DO UPDATE 
                        SET common_name = excluded.common_name
                    |]
    let values =
          (\(AnnotatedStopPointRef id commonName) -> (id, commonName))
            <$> stopPoints
    void $ executeMany connection statement values

  insertRouteSections :: Connection -> [RouteSection] -> IO ()
  insertRouteSections connection routeSections = do
    let statement = [sql| 
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
    let statement = [sql| 
                      INSERT INTO route_links (route_link_id, route_section_id, from_stop_point, to_stop_point, route_direction) 
                      VALUES (?, ?, ?, ?, ?)
                      ON CONFLICT (route_link_id) DO UPDATE 
                        SET route_section_id = excluded.route_section_id,
                            from_stop_point = excluded.from_stop_point,
                            to_stop_point = excluded.to_stop_point, 
                            route_direction = excluded.route_direction
                    |]
    let values =
          (\(RouteLink routeLinkID fromStopPoint toStopPoint direction) ->
              (routeLinkID, routeSectionID, fromStopPoint, toStopPoint, direction)
            )
            <$> routeLinks
    void $ executeMany connection statement values

  insertRoutes :: Connection -> [Route] -> IO ()
  insertRoutes connection routes = do
    let statement = [sql| 
                      INSERT INTO routes (route_id, route_description, route_section_id) 
                      VALUES (?, ?, ?)
                      ON CONFLICT (route_id) DO UPDATE 
                        SET route_description = excluded.route_description,
                            route_section_id = excluded.route_section_id
                    |]
    let values =
          (\(Route routeID routeDescription routeSectionID) ->
              (routeID, routeDescription, routeSectionID)
            )
            <$> routes
    void $ executeMany connection statement values

  insertJourneyPatternSections :: Connection -> [JourneyPatternSection] -> IO ()
  insertJourneyPatternSections connection journeyPatternSections = do
    let statement = [sql| 
                      INSERT INTO journey_pattern_sections (journey_pattern_section_id) 
                      VALUES (?)
                      ON CONFLICT (journey_pattern_section_id) DO NOTHING
                    |]
    let values =
          (\(JourneyPatternSection journeyPatterSectionID _) ->
              Only journeyPatterSectionID
            )
            <$> journeyPatternSections
    void $ executeMany connection statement values
    forM_ journeyPatternSections
      $ \(JourneyPatternSection journeyPatterSectionID timingLinks) ->
          insertJourneyPatternTimingLinks connection
                                          journeyPatterSectionID
                                          timingLinks

  insertJourneyPatternTimingLinks
    :: Connection -> String -> [JourneyPatternTimingLink] -> IO ()
  insertJourneyPatternTimingLinks connection journeyPatterSectionID timingLinks
    = do
      let statement = [sql| 
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
      let
        values =
          (\(JourneyPatternTimingLink journeyPatternTimingLinkId journeyPatternFromWaitTime journeyPatternFromStopPointRef journeyPatternFromTimingStatus journeyPatternToStopPointsRef journeyPatternToTimingStatus routeLinkRef journeyDirection runTime) ->
              ( journeyPatternTimingLinkId
              , journeyPatterSectionID
              , journeyPatternFromStopPointRef
              , journeyPatternFromTimingStatus
              , journeyPatternFromWaitTime
              , journeyPatternToStopPointsRef
              , journeyPatternToTimingStatus
              , routeLinkRef
              , journeyDirection
              , runTime
              )
            )
            <$> timingLinks
      void $ executeMany connection statement values

  insertOperators :: Connection -> [Operator] -> IO ()
  insertOperators connection operators = do
    let statement = [sql| 
                      INSERT INTO operators (operator_id, national_operator_code, operator_code, operator_short_name) 
                      VALUES (?, ?, ?, ?)
                      ON CONFLICT (operator_id) DO UPDATE 
                        SET national_operator_code = excluded.national_operator_code,
                            operator_code = excluded.operator_code,
                            operator_short_name = excluded.operator_short_name
                    |]
    let
      values =
        (\(Operator operatorID nationalOperatorCode operatorCode operatorShortName) ->
            (operatorID, nationalOperatorCode, operatorCode, operatorShortName)
          )
          <$> operators
    void $ executeMany connection statement values

  insertServices :: Connection -> [TransxchangeTypes.Service] -> IO ()
  insertServices connection services = do
    let statement = [sql| 
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
    let
      values =
        (\(TransxchangeTypes.Service serviceCode lines (DateRange startDate endDate) registeredOperatorRef mode description (StandardService origin destination _)) ->
            ( serviceCode
            , registeredOperatorRef
            , mode
            , description
            , startDate
            , endDate
            , origin
            , destination
            )
          )
          <$> services
    void $ executeMany connection statement values
    forM_ services
      $ \TransxchangeTypes.Service { serviceCode = serviceCode, TransxchangeTypes.lines = lines, standardService = StandardService { journeyPatterns = journeyPatterns } } ->
          do
            insertLines connection serviceCode lines
            insertJourneyPatterns connection serviceCode journeyPatterns

  insertLines :: Connection -> String -> [Line] -> IO ()
  insertLines connection serviceCode lines = do
    let statement = [sql| 
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
    let statement = [sql| 
                      INSERT INTO journey_patterns (journey_pattern_id, service_code, journey_pattern_section_id, direction) 
                      VALUES (?, ?, ?, ?)
                      ON CONFLICT (journey_pattern_id) DO UPDATE 
                        SET service_code = excluded.service_code,
                            journey_pattern_section_id = excluded.journey_pattern_section_id,
                            direction = excluded.direction
                    |]
    let
      values =
        (\(JourneyPattern journeyPatternID journeyPatternDirection journeyPatternSectionRef) ->
            ( journeyPatternID
            , serviceCode
            , journeyPatternSectionRef
            , journeyPatternDirection
            )
          )
          <$> journeyPatterns
    void $ executeMany connection statement values

  insertVehicleJourneys :: Connection -> [VehicleJourney] -> IO ()
  insertVehicleJourneys connection vehicleJourneys = do
    let statement = [sql| 
                      INSERT INTO vehicle_journeys (vehicle_journey_code, service_code, line_id, journey_pattern_id, operator_id, days_of_week, departure_time, note, note_code) 
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                      ON CONFLICT (vehicle_journey_code) DO UPDATE 
                        SET service_code = excluded.service_code,
                            line_id = excluded.line_id,
                            journey_pattern_id = excluded.journey_pattern_id,
                            operator_id = excluded.operator_id,
                            days_of_week = excluded.days_of_week,
                            departure_time = excluded.departure_time,
                            note = excluded.note,
                            note_code = excluded.note_code
                    |]
    let
      values =
        (\(VehicleJourney operatorRef vehicleJourneyCode serviceRef lineRef journeyPatternRef departureTime daysOfWeek _ _ note noteCode daysOfNonOperationServicedOrganisationRef) ->
            ( vehicleJourneyCode
            , serviceRef
            , lineRef
            , journeyPatternRef
            , operatorRef
            , PGArray daysOfWeek
            , timeStringToTime departureTime
            , note
            , noteCode
            )
          )
          <$> vehicleJourneys
    void $ executeMany connection statement values
    forM_ vehicleJourneys
      $ \VehicleJourney { vehicleJourneyCode = vehicleJourneyCode, specialDaysOfOperation = specialDaysOfOperation, specialDaysOfNonOperation = specialDaysOfNonOperation } ->
          do
            insertDayOfOperation connection
                                 vehicleJourneyCode
                                 specialDaysOfOperation
            insertDayOfNonOperation connection
                                    vehicleJourneyCode
                                    specialDaysOfNonOperation

  insertDayOfOperation :: Connection -> String -> [DateRange] -> IO ()
  insertDayOfOperation connection vehicleJourneyCode daysOfOperation = do
    let statement = [sql| 
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
    let statement = [sql| 
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
    in  TimeOfDay (read $ head timeParts)
                  (read $ timeParts !! 1)
                  (read $ timeParts !! 2)
