{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database.Weather
  ( insertLocationWeather,
    getLocationWeathers,
  )
where

import App.Env (Application)
import Control.Monad (void)
import Database.Connection (withConnection)
import Database.PostgreSQL.Simple
  ( execute,
    query_,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Types (LocationWeather)
import Types.Weather

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
