{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.Runner
  ( SentryConfig (SentryConfig),
    runRepeatedAction,
  )
import TimetableDocumentScraper (fetchTimetableDocuments)

main :: IO ()
main =
  runRepeatedAction
    (SentryConfig "TIMETABLE_DOCUMENT_SCRAPER_SENTRY_DSN" "timetable-document-scraper-logger")
    "Fetching timetable documents"
    (6 * 60 * 60 * 1000 * 1000)
    fetchTimetableDocuments
