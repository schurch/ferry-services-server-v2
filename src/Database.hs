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
  , getServiceLocations
  , deleteInstallationWithID
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
                                                , executeMany
                                                , query
                                                , query_
                                                , close
                                                , connectPostgreSQL
                                                , Only(Only)
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           System.Environment             ( getEnv )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Types                          ( ServiceLocation
                                                , Installation
                                                , Service
                                                , DeviceType
                                                )
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

getServiceLocations :: MonadIO m => m [ServiceLocation]
getServiceLocations = withConnection $ \connection -> query_
  connection
  [sql| 
    SELECT sl.service_id, l.location_id, l.name, l.latitude, l.longitude
    FROM service_locations sl
    JOIN locations l ON l.location_id = sl.location_id 
  |]

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

updateTransxchangeData :: MonadIO m => TransXChangeData -> m ()
updateTransxchangeData TransXChangeData { stopPoints = stopPoints, routeSections = routeSections, routes = routes, journeyPatternSections = journeyPatternSections, operators = operators }
  = do
    void $ withConnection $ \connection -> do
      insertStopPoints connection stopPoints
      insertRouteSections connection routeSections
      insertRoutes connection routes
      insertJourneyPatternSections connection journeyPatternSections
      insertOperators connection operators
 where
  insertStopPoints :: Connection -> [AnnotatedStopPointRef] -> IO ()
  insertStopPoints connection stopPoints = do
    let statement = [sql| 
                      INSERT INTO stop_point (stop_point_id, common_name) 
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
                      INSERT INTO route_section (route_section_id) 
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
                      INSERT INTO route_link (route_link_id, route_section_id, from_stop_point, to_stop_point, route_direction) 
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
                      INSERT INTO route (route_id, route_description, route_section_id) 
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
                      INSERT INTO journey_pattern_section (journey_pattern_section_id) 
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
                      INSERT INTO journey_pattern_timing_link (
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
                      INSERT INTO operator (operator_id, national_operator_code, operator_code, operator_short_name) 
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
