{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.Runner
  ( SentryConfig (SentryConfig),
    runRepeatedActions,
  )
import Scraper

main :: IO ()
main =
  runRepeatedActions
    (SentryConfig "SCRAPER_SENTRY_DSN" "scraper-logger")
    "Fetching statuses"
    (15 * 60 * 1000 * 1000)
    [ fetchCalMacStatusesAndNotify,
      fetchCorranFerryAndNotify,
      fetchNorthLinkServicesAndNotify,
      fetchPentlandFerriesAndNotify,
      fetchWesternFerriesAndNotify,
      fetchShetlandFerriesAndNotify,
      fetchOrkneyFerriesAndNotify
    ]
