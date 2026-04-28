{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.Runner
  ( SentryConfig (SentryConfig),
    runRepeatedAction,
  )
import RailDepartureFetcher
  ( fetchRailDepartures,
  )

main :: IO ()
main =
  runRepeatedAction
    (SentryConfig "RAIL_DEPARTURE_FETCHER_SENTRY_DSN" "rail-departure-fetcher-logger")
    "Fetching rail departures"
    (1 * 60 * 1000 * 1000)
    fetchRailDepartures
