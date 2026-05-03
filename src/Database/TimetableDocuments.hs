{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Database.TimetableDocuments
  ( getTimetableDocumentServiceLinks,
    getTimetableDocuments,
    saveTimetableDocuments,
  )
where

import App.Env (Application)
import Control.Monad (forM_, void)
import Data.Maybe (listToMaybe)
import Database.Connection (withConnection)
import Database.PostgreSQL.Simple
  ( Only (Only),
    execute,
    executeMany,
    query,
    query_,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Types
  ( ScrapedTimetableDocument (..),
    TimetableDocument,
  )

getTimetableDocuments :: Maybe Int -> Application [TimetableDocument]
getTimetableDocuments serviceID = withConnection $ \connection ->
  case serviceID of
    Nothing ->
      query_
        connection
        [sql|
          SELECT
            td.timetable_document_id,
            td.organisation_id,
            o.name,
            td.title,
            td.source_url,
            td.content_hash,
            td.content_type,
            td.content_length,
            td.last_seen_at,
            td.updated,
            td.created
          FROM timetable_documents td
          JOIN organisations o ON o.organisation_id = td.organisation_id
          ORDER BY o.name, td.title
        |]
    Just selectedServiceID ->
      query
        connection
        [sql|
          SELECT
            td.timetable_document_id,
            td.organisation_id,
            o.name,
            td.title,
            td.source_url,
            td.content_hash,
            td.content_type,
            td.content_length,
            td.last_seen_at,
            td.updated,
            td.created
          FROM timetable_documents td
          JOIN organisations o ON o.organisation_id = td.organisation_id
          JOIN timetable_document_services tds
            ON tds.timetable_document_id = td.timetable_document_id
          WHERE tds.service_id = ?
          ORDER BY o.name, td.title
        |]
        (Only selectedServiceID)

getTimetableDocumentServiceLinks :: Application [(Int, Int)]
getTimetableDocumentServiceLinks = withConnection $ \connection ->
  query_
    connection
    [sql|
      SELECT timetable_document_id, service_id
      FROM timetable_document_services
      ORDER BY timetable_document_id, service_id
    |]

saveTimetableDocuments :: [ScrapedTimetableDocument] -> Application ()
saveTimetableDocuments documents = withConnection $ \connection ->
  forM_ documents $ \ScrapedTimetableDocument {..} -> do
    rows <-
      query
        connection
        [sql|
          INSERT INTO timetable_documents (
            organisation_id,
            title,
            source_url,
            content_hash,
            content_type,
            content_length,
            last_seen_at
          )
          VALUES (?, ?, ?, ?, ?, ?, ?)
          ON CONFLICT (source_url) DO UPDATE
            SET organisation_id = excluded.organisation_id,
                title = excluded.title,
                content_hash = excluded.content_hash,
                content_type = excluded.content_type,
                content_length = excluded.content_length,
                last_seen_at = excluded.last_seen_at,
                updated = CURRENT_TIMESTAMP
          RETURNING timetable_document_id
        |]
        ( scrapedTimetableDocumentOrganisationID,
          scrapedTimetableDocumentTitle,
          scrapedTimetableDocumentSourceURL,
          scrapedTimetableDocumentContentHash,
          scrapedTimetableDocumentContentType,
          scrapedTimetableDocumentContentLength,
          scrapedTimetableDocumentLastSeenAt
        )
    case listToMaybe rows of
      Nothing -> pure ()
      Just (Only documentID) -> do
        void $
          execute
            connection
            [sql|
              DELETE FROM timetable_document_services
              WHERE timetable_document_id = ?
            |]
            (Only (documentID :: Int))
        void $
          executeMany
            connection
            [sql|
              INSERT INTO timetable_document_services (timetable_document_id, service_id)
              VALUES (?, ?)
              ON CONFLICT DO NOTHING
            |]
            [(documentID, serviceID) | serviceID <- scrapedTimetableDocumentServiceIDs]
