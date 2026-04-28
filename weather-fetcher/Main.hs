{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.Runner
  ( SentryConfig (SentryConfig),
    runRepeatedAction,
  )
import WeatherFetcher (fetchWeather)

main :: IO ()
main =
  runRepeatedAction
    (SentryConfig "WEATHER_FETCHER_SENTRY_DSN" "weather-fetcher-logger")
    "Fetching weather"
    (15 * 60 * 1000 * 1000)
    fetchWeather
