{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.Runner
  ( SentryConfig (SentryConfig),
    runRepeatedAction,
  )
import VesselFetcher
  ( defaultMmsis,
    fetchVessels,
  )

main :: IO ()
main =
  runRepeatedAction
    (SentryConfig "VESSEL_FETCHER_SENTRY_DSN" "vessel-fetcher-logger")
    "Fetching vessels"
    (5 * 60 * 1000 * 1000)
    (fetchVessels defaultMmsis)
