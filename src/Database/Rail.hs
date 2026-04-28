{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Rail
  ( insertRailDepartureFetcherResult,
    getLocationRailDepartures,
  )
where

import App.Env (Application)
import Control.Monad (void)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay (..))
import Database.Connection (withConnection)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    execute,
    executeMany,
    query,
    withTransaction,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Types (LocationRailDeparture)
import Types.Rail
import Utility (splitOn)

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
    deleteOldData connection railDeparturesFetcherResult =
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

timeStringToTime :: String -> TimeOfDay
timeStringToTime string =
  let timeParts = splitOn ':' string
   in case timeParts of
        [h, m, s] -> TimeOfDay (read h) (read m) (read s)
        [h, m] -> TimeOfDay (read h) (read m) 0
        _ -> error $ "Unable to convert '" <> string <> "' to TimeOfDay"
