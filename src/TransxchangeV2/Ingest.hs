{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module TransxchangeV2.Ingest
  ( ingestDirectoryV2,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (withTransaction)
import System.Logger
  ( Level (Info),
    Logger,
  )
import qualified System.Logger as Logger
import System.Logger.Class (info)
import System.Logger.Message (msg)
import TransxchangeV2.Parser
  ( findTransxchangeFilesV2,
    fileMayContainFerryServiceV2,
    parseTransxchangeFileV2,
  )
import TransxchangeV2.Repository
  ( clearTx2Tables,
    insertTx2Document,
  )
import TransxchangeV2.Transform (filterFerryDocuments)
import TransxchangeV2.Types
import Types (Application, Env (connectionPool, logger))

ingestDirectoryV2 :: FilePath -> Application Tx2IngestSummary
ingestDirectoryV2 directory = do
  info (msg @String $ "TransXChange V2 ingest from directory: " <> directory)
  files <- liftIO $ findTransxchangeFilesV2 directory
  info (msg @String $ "TransXChange V2 files discovered: " <> show (length files))
  pool <- asks connectionPool
  appLogger <- asks logger
  liftIO $
    withResource pool $ \connection ->
      withTransaction connection $ do
        clearTx2Tables connection
        go appLogger connection emptySummary 1 (length files) files
  where
    emptySummary =
      Tx2IngestSummary
        { tx2FilesParsed = 0,
          tx2FilesSkipped = 0,
          tx2FilesFailed = 0,
          tx2FerryDocuments = 0,
          tx2ServicesWritten = 0,
          tx2JourneyWritten = 0
        }

    go _ _ !summary _ _ [] = return summary
    go appLogger connection !summary !index !totalFiles (filePath : remainingFiles) = do
      logProgress appLogger index totalFiles filePath summary
      mayContainFerry <- fileMayContainFerryServiceV2 filePath
      if not mayContainFerry
        then do
          let !nextSummary =
                summary
                  { tx2FilesSkipped = tx2FilesSkipped summary + 1
                  }
          go appLogger connection nextSummary (index + 1) totalFiles remainingFiles
        else do
          parsed <- parseTransxchangeFileV2 filePath
          case parsed of
            Left _ ->
              let !nextSummary =
                    summary
                      { tx2FilesFailed = tx2FilesFailed summary + 1
                      }
               in go appLogger connection nextSummary (index + 1) totalFiles remainingFiles
            Right document -> do
              let ferryDocuments = filterFerryDocuments [document]
              mapM_ (insertTx2Document connection) ferryDocuments
              let ferryDocumentCount = length ferryDocuments
              let serviceCount = sum $ fmap (length . tx2Services) ferryDocuments
              let journeyCount = sum $ fmap (length . tx2VehicleJourneys) ferryDocuments
              let !nextSummary =
                    summary
                      { tx2FilesParsed = tx2FilesParsed summary + 1,
                        tx2FerryDocuments = tx2FerryDocuments summary + ferryDocumentCount,
                        tx2ServicesWritten = tx2ServicesWritten summary + serviceCount,
                        tx2JourneyWritten = tx2JourneyWritten summary + journeyCount
                      }
              go appLogger connection nextSummary (index + 1) totalFiles remainingFiles

    logProgress :: Logger -> Int -> Int -> FilePath -> Tx2IngestSummary -> IO ()
    logProgress appLogger index totalFiles filePath summary =
      if shouldLogProgress index totalFiles
        then
          Logger.log
            appLogger
            Info
            ( msg @String $
                "TransXChange V2 progress "
                  <> show index
                  <> "/"
                  <> show totalFiles
                  <> ": "
                  <> filePath
                  <> " (parsed="
                  <> show (tx2FilesParsed summary)
                  <> ", skipped="
                  <> show (tx2FilesSkipped summary)
                  <> ", failed="
                  <> show (tx2FilesFailed summary)
                  <> ", ferry_documents="
                  <> show (tx2FerryDocuments summary)
                  <> ", services_written="
                  <> show (tx2ServicesWritten summary)
                  <> ", vehicle_journeys_written="
                  <> show (tx2JourneyWritten summary)
                  <> ")"
            )
        else return ()

    shouldLogProgress :: Int -> Int -> Bool
    shouldLogProgress index totalFiles =
      index == 1 || index == totalFiles || index `mod` 50 == 0
