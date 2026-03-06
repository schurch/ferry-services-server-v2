{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Trans.Reader (runReaderT)
import Data.Pool (createPool)
import Data.String (fromString)
import Database.PostgreSQL.Simple
  ( close,
    connectPostgreSQL,
  )
import System.Environment (getArgs, getEnv)
import System.Logger
  ( Output (StdOut),
    create,
    info,
  )
import System.Logger.Message (msg)
import TransxchangeV2.Ingest (ingestDirectoryV2)
import TransxchangeV2.Types (Tx2IngestSummary (..))
import Types (Env (Env))

main :: IO ()
main = do
  args <- getArgs
  let directory =
        case args of
          (dir : _) -> dir
          [] -> "S"
  logger <- create StdOut
  connectionString <- getEnv "DB_CONNECTION"
  connectionPool <-
    createPool
      (connectPostgreSQL $ fromString connectionString)
      Database.PostgreSQL.Simple.close
      2
      60
      10
  summary <- runReaderT (ingestDirectoryV2 directory) (Env logger connectionPool)
  info logger (msg @String $ "TransXChange v2 ingest complete from: " <> directory)
  info logger (msg @String $ renderSummary summary)

renderSummary :: Tx2IngestSummary -> String
renderSummary summary =
  "files_parsed="
    <> show (tx2FilesParsed summary)
    <> ", files_skipped="
    <> show (tx2FilesSkipped summary)
    <> ", files_failed="
    <> show (tx2FilesFailed summary)
    <> ", ferry_documents="
    <> show (tx2FerryDocuments summary)
    <> ", services_written="
    <> show (tx2ServicesWritten summary)
    <> ", vehicle_journeys_written="
    <> show (tx2JourneyWritten summary)
