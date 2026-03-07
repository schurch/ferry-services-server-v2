{-# LANGUAGE QuasiQuotes #-}

module TransxchangeV2.Repository
  ( clearTx2Tables,
    insertTx2Document,
    replaceTx2Data,
  )
where

import Control.Monad (void)
import Data.Int (Int64)
import Data.List (nub)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    executeMany,
    execute_,
    query,
    withTransaction,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import TransxchangeV2.Types

replaceTx2Data :: Connection -> [Tx2Document] -> IO ()
replaceTx2Data connection documents =
  withTransaction connection $ do
    clearTx2Tables connection
    mapM_ (insertTx2Document connection) documents

clearTx2Tables :: Connection -> IO ()
clearTx2Tables connection =
  void $
    execute_
      connection
      [sql|
        TRUNCATE TABLE
          tx2_vehicle_journey_bank_holiday_non_operation_rules,
          tx2_vehicle_journey_bank_holiday_operation_rules,
          tx2_vehicle_journey_days_of_non_operation,
          tx2_vehicle_journey_days_of_operation,
          tx2_vehicle_journey_days,
          tx2_vehicle_journeys,
          tx2_journey_pattern_timing_links,
          tx2_journey_pattern_sections,
          tx2_journey_patterns,
          tx2_lines,
          tx2_stop_points,
          tx2_services,
          tx2_documents
      |]

insertTx2Document :: Connection -> Tx2Document -> IO ()
insertTx2Document connection document = do
  documentId <- insertDocument connection document
  insertStopPoints connection documentId (tx2StopPoints document)
  insertServices connection documentId (tx2Services document)
  insertLines connection documentId (tx2Lines document)
  insertJourneyPatterns connection documentId (tx2JourneyPatterns document)
  insertJourneyPatternSections connection documentId (tx2JourneyPatternSections document)
  insertJourneyPatternTimingLinks connection documentId (tx2JourneyPatternTimingLinks document)
  insertVehicleJourneys connection documentId vehicleJourneys
  insertVehicleJourneyDays connection documentId vehicleJourneys
  insertVehicleJourneyDaysOfOperation connection documentId vehicleJourneys
  insertVehicleJourneyDaysOfNonOperation connection documentId vehicleJourneys
  insertVehicleJourneyBankHolidayOperationRules connection documentId vehicleJourneys
  insertVehicleJourneyBankHolidayNonOperationRules connection documentId vehicleJourneys
  where
    vehicleJourneys = tx2VehicleJourneys document

insertDocument :: Connection -> Tx2Document -> IO Int64
insertDocument connection document = do
  [Only documentId] <-
    query
      connection
      [sql|
        INSERT INTO tx2_documents (
          source_path,
          source_file_name,
          source_version_key,
          source_creation_datetime,
          source_modification_datetime
        )
        VALUES (?, ?, ?, ?, ?)
        RETURNING document_id
      |]
      ( tx2SourcePath document,
        tx2SourceFileName document,
        tx2SourceVersionKey document,
        tx2SourceCreationDateTime document,
        tx2SourceModificationDateTime document
      )
  return documentId

insertServices :: Connection -> Int64 -> [Tx2Service] -> IO ()
insertServices connection documentId services = do
  let statement =
        [sql|
          INSERT INTO tx2_services (
            document_id,
            service_code,
            operator_ref,
            mode,
            description,
            origin,
            destination,
            start_date,
            end_date
          )
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
          ON CONFLICT (document_id, service_code) DO UPDATE
            SET operator_ref = excluded.operator_ref,
                mode = excluded.mode,
                description = excluded.description,
                origin = excluded.origin,
                destination = excluded.destination,
                start_date = excluded.start_date,
                end_date = excluded.end_date
        |]
  let values =
        fmap
          ( \service ->
              ( documentId,
                tx2ServiceCode service,
                tx2OperatorRef service,
                tx2Mode service,
                tx2Description service,
                tx2Origin service,
                tx2Destination service,
                tx2StartDate service,
                tx2EndDate service
              )
          )
          services
  void $ executeMany connection statement values

insertStopPoints :: Connection -> Int64 -> [Tx2StopPoint] -> IO ()
insertStopPoints connection documentId stopPoints = do
  let statement =
        [sql|
          INSERT INTO tx2_stop_points (
            document_id,
            stop_point_ref,
            common_name
          )
          VALUES (?, ?, ?)
          ON CONFLICT (document_id, stop_point_ref) DO UPDATE
            SET common_name = excluded.common_name
        |]
  let values =
        fmap
          ( \stopPoint ->
              ( documentId,
                tx2StopPointRef stopPoint,
                tx2StopPointCommonName stopPoint
              )
          )
          stopPoints
  void $ executeMany connection statement values

insertLines :: Connection -> Int64 -> [Tx2Line] -> IO ()
insertLines connection documentId lines = do
  let statement =
        [sql|
          INSERT INTO tx2_lines (document_id, line_id, service_code, line_name)
          VALUES (?, ?, ?, ?)
          ON CONFLICT (document_id, line_id) DO UPDATE
            SET service_code = excluded.service_code,
                line_name = excluded.line_name
        |]
  let values =
        fmap
          (\line -> (documentId, tx2LineId line, tx2LineServiceCode line, tx2LineName line))
          lines
  void $ executeMany connection statement values

insertJourneyPatterns :: Connection -> Int64 -> [Tx2JourneyPattern] -> IO ()
insertJourneyPatterns connection documentId journeyPatterns = do
  let statement =
        [sql|
          INSERT INTO tx2_journey_patterns (
            document_id,
            journey_pattern_id,
            service_code,
            direction
          )
          VALUES (?, ?, ?, ?)
          ON CONFLICT (document_id, journey_pattern_id) DO UPDATE
            SET service_code = excluded.service_code,
                direction = excluded.direction
        |]
  let values =
        fmap
          ( \pattern ->
              ( documentId,
                tx2JourneyPatternId pattern,
                tx2JourneyPatternServiceCode pattern,
                tx2JourneyPatternDirection pattern
              )
          )
          journeyPatterns
  void $ executeMany connection statement values

insertJourneyPatternSections :: Connection -> Int64 -> [Tx2JourneyPatternSection] -> IO ()
insertJourneyPatternSections connection documentId sections = do
  let statement =
        [sql|
          INSERT INTO tx2_journey_pattern_sections (
            document_id,
            journey_pattern_id,
            section_ref,
            section_order
          )
          VALUES (?, ?, ?, ?)
          ON CONFLICT (document_id, journey_pattern_id, section_order) DO UPDATE
            SET section_ref = excluded.section_ref
        |]
  let values =
        fmap
          ( \section ->
              ( documentId,
                tx2JourneyPatternSectionPatternId section,
                tx2JourneyPatternSectionRef section,
                tx2JourneyPatternSectionOrder section
              )
          )
          sections
  void $ executeMany connection statement values

insertVehicleJourneys :: Connection -> Int64 -> [Tx2VehicleJourney] -> IO ()
insertVehicleJourneys connection documentId vehicleJourneys = do
  let statement =
        [sql|
          INSERT INTO tx2_vehicle_journeys (
            document_id,
            vehicle_journey_code,
            service_code,
            line_id,
            journey_pattern_id,
            operator_ref,
            departure_time,
            note,
            note_code
          )
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
          ON CONFLICT (document_id, vehicle_journey_code) DO UPDATE
            SET service_code = excluded.service_code,
                line_id = excluded.line_id,
                journey_pattern_id = excluded.journey_pattern_id,
                operator_ref = excluded.operator_ref,
                departure_time = excluded.departure_time,
                note = excluded.note,
                note_code = excluded.note_code
        |]
  let values =
        fmap
          ( \journey ->
              ( documentId,
                tx2VehicleJourneyCode journey,
                tx2VehicleJourneyServiceCode journey,
                tx2VehicleJourneyLineId journey,
                tx2VehicleJourneyPatternId journey,
                tx2VehicleJourneyOperatorRef journey,
                tx2VehicleJourneyDepartureTime journey,
                tx2VehicleJourneyNote journey,
                tx2VehicleJourneyNoteCode journey
              )
          )
          vehicleJourneys
  void $ executeMany connection statement values

insertVehicleJourneyDays :: Connection -> Int64 -> [Tx2VehicleJourney] -> IO ()
insertVehicleJourneyDays connection documentId vehicleJourneys = do
  let statement =
        [sql|
          INSERT INTO tx2_vehicle_journey_days (document_id, vehicle_journey_code, day_rule)
          VALUES (?, ?, ?)
          ON CONFLICT (document_id, vehicle_journey_code, day_rule) DO NOTHING
        |]
  let values =
        concatMap
          ( \journey ->
              fmap
                (\dayRule -> (documentId, tx2VehicleJourneyCode journey, dayRule))
                (nub $ fmap dayRuleToText (tx2VehicleJourneyDayRules journey))
          )
          vehicleJourneys
  void $ executeMany connection statement values

insertVehicleJourneyDaysOfOperation :: Connection -> Int64 -> [Tx2VehicleJourney] -> IO ()
insertVehicleJourneyDaysOfOperation connection documentId vehicleJourneys = do
  let statement =
        [sql|
          INSERT INTO tx2_vehicle_journey_days_of_operation (
            document_id,
            vehicle_journey_code,
            start_date,
            end_date
          )
          VALUES (?, ?, ?, ?)
          ON CONFLICT (document_id, vehicle_journey_code, start_date, end_date) DO NOTHING
        |]
  let values =
        concatMap
          ( \journey ->
              fmap
                ( \dateRange ->
                    ( documentId,
                      tx2VehicleJourneyCode journey,
                      tx2DateRangeStart dateRange,
                      tx2DateRangeEnd dateRange
                    )
                )
                (nub $ tx2VehicleJourneyDaysOfOperation journey)
          )
          vehicleJourneys
  void $ executeMany connection statement values

insertVehicleJourneyDaysOfNonOperation :: Connection -> Int64 -> [Tx2VehicleJourney] -> IO ()
insertVehicleJourneyDaysOfNonOperation connection documentId vehicleJourneys = do
  let statement =
        [sql|
          INSERT INTO tx2_vehicle_journey_days_of_non_operation (
            document_id,
            vehicle_journey_code,
            start_date,
            end_date
          )
          VALUES (?, ?, ?, ?)
          ON CONFLICT (document_id, vehicle_journey_code, start_date, end_date) DO NOTHING
        |]
  let values =
        concatMap
          ( \journey ->
              fmap
                ( \dateRange ->
                    ( documentId,
                      tx2VehicleJourneyCode journey,
                      tx2DateRangeStart dateRange,
                      tx2DateRangeEnd dateRange
                    )
                )
                (nub $ tx2VehicleJourneyDaysOfNonOperation journey)
          )
          vehicleJourneys
  void $ executeMany connection statement values

insertVehicleJourneyBankHolidayOperationRules :: Connection -> Int64 -> [Tx2VehicleJourney] -> IO ()
insertVehicleJourneyBankHolidayOperationRules connection documentId vehicleJourneys = do
  let statement =
        [sql|
          INSERT INTO tx2_vehicle_journey_bank_holiday_operation_rules (
            document_id,
            vehicle_journey_code,
            bank_holiday_rule
          )
          VALUES (?, ?, ?)
          ON CONFLICT (document_id, vehicle_journey_code, bank_holiday_rule) DO NOTHING
        |]
  let values =
        concatMap
          ( \journey ->
              fmap
                (\rule -> (documentId, tx2VehicleJourneyCode journey, rule))
                (nub $ tx2VehicleJourneyBankHolidayOperationRules journey)
          )
          vehicleJourneys
  void $ executeMany connection statement values

insertVehicleJourneyBankHolidayNonOperationRules :: Connection -> Int64 -> [Tx2VehicleJourney] -> IO ()
insertVehicleJourneyBankHolidayNonOperationRules connection documentId vehicleJourneys = do
  let statement =
        [sql|
          INSERT INTO tx2_vehicle_journey_bank_holiday_non_operation_rules (
            document_id,
            vehicle_journey_code,
            bank_holiday_rule
          )
          VALUES (?, ?, ?)
          ON CONFLICT (document_id, vehicle_journey_code, bank_holiday_rule) DO NOTHING
        |]
  let values =
        concatMap
          ( \journey ->
              fmap
                (\rule -> (documentId, tx2VehicleJourneyCode journey, rule))
                (nub $ tx2VehicleJourneyBankHolidayNonOperationRules journey)
          )
          vehicleJourneys
  void $ executeMany connection statement values

insertJourneyPatternTimingLinks :: Connection -> Int64 -> [Tx2JourneyPatternTimingLink] -> IO ()
insertJourneyPatternTimingLinks connection documentId timingLinks = do
  let statement =
        [sql|
          INSERT INTO tx2_journey_pattern_timing_links (
            document_id,
            journey_pattern_timing_link_id,
            journey_pattern_section_ref,
            sort_order,
            from_stop_point_ref,
            to_stop_point_ref,
            route_link_ref,
            direction,
            run_time,
            from_wait_time
          )
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
          ON CONFLICT (document_id, journey_pattern_timing_link_id) DO UPDATE
            SET journey_pattern_section_ref = excluded.journey_pattern_section_ref,
                sort_order = excluded.sort_order,
                from_stop_point_ref = excluded.from_stop_point_ref,
                to_stop_point_ref = excluded.to_stop_point_ref,
                route_link_ref = excluded.route_link_ref,
                direction = excluded.direction,
                run_time = excluded.run_time,
                from_wait_time = excluded.from_wait_time
        |]
  let values =
        fmap
          ( \timingLink ->
              ( documentId,
                tx2JourneyPatternTimingLinkId timingLink,
                tx2JourneyPatternTimingLinkSectionRef timingLink,
                tx2JourneyPatternTimingLinkSortOrder timingLink,
                tx2JourneyPatternTimingLinkFromStopPointRef timingLink,
                tx2JourneyPatternTimingLinkToStopPointRef timingLink,
                tx2JourneyPatternTimingLinkRouteLinkRef timingLink,
                tx2JourneyPatternTimingLinkDirection timingLink,
                tx2JourneyPatternTimingLinkRunTime timingLink,
                tx2JourneyPatternTimingLinkFromWaitTime timingLink
              )
          )
          timingLinks
  void $ executeMany connection statement values

dayRuleToText :: Tx2DayRule -> String
dayRuleToText value =
  case value of
    Tx2Monday -> "monday"
    Tx2Tuesday -> "tuesday"
    Tx2Wednesday -> "wednesday"
    Tx2Thursday -> "thursday"
    Tx2Friday -> "friday"
    Tx2Saturday -> "saturday"
    Tx2Sunday -> "sunday"
    Tx2MondayToFriday -> "monday_to_friday"
    Tx2MondayToSaturday -> "monday_to_saturday"
    Tx2MondayToSunday -> "monday_to_sunday"
    Tx2Weekend -> "weekend"
    Tx2NotMonday -> "not_monday"
    Tx2NotTuesday -> "not_tuesday"
    Tx2NotWednesday -> "not_wednesday"
    Tx2NotThursday -> "not_thursday"
    Tx2NotFriday -> "not_friday"
    Tx2NotSaturday -> "not_saturday"
    Tx2NotSunday -> "not_sunday"
    Tx2HolidaysOnly -> "holidays_only"
