{-# LANGUAGE OverloadedStrings #-}

module OfflineSnapshotSpec where

import App.Database (createConnectionPool)
import App.Env (Env (Env))
import App.Logger (Output (StdOut), create)
import Control.Monad.Reader (runReaderT)
import Data.List (find, isPrefixOf)
import Database.SQLite.Simple
  ( Only (Only),
    close,
    fromOnly,
    open,
    query_,
  )
import OfflineSnapshot
  ( generateOfflineSnapshot,
    writeOfflineSnapshot,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (lookupEnv, setEnv)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
  )

spec :: Spec
spec =
  describe "SQLite offline snapshot" $ do
    it "writes a client queryable SQLite database" $ do
      snapshotPath <- snapshotFixturePath
      metadataPath <- metadataFixturePath
      removeIfExists snapshotPath
      removeIfExists metadataPath

      logger <- create StdOut
      connectionString <- getDbConnectionString
      connectionPool <- createConnectionPool connectionString
      snapshot <- runReaderT generateOfflineSnapshot (Env logger connectionPool)
      _ <- writeOfflineSnapshot snapshotPath metadataPath snapshot

      connection <- open snapshotPath
      viewRows <- query_ connection "SELECT name FROM sqlite_master WHERE type = 'view' ORDER BY name" :: IO [Only String]
      schemaVersion <- query_ connection "SELECT value FROM metadata WHERE key = 'schema_version'" :: IO [Only String]
      close connection

      let views = fmap fromOnly viewRows
      views `shouldSatisfy` \actualViews ->
        all (`elem` actualViews) ["client_services", "client_service_locations", "client_departures"]
      schemaVersion `shouldBe` [Only ("1" :: String)]

snapshotFixturePath :: IO FilePath
snapshotFixturePath = do
  createDirectoryIfMissing True "offline"
  pure "offline/test-snapshot.sqlite3"

metadataFixturePath :: IO FilePath
metadataFixturePath = do
  createDirectoryIfMissing True "offline"
  pure "offline/test-snapshot.meta.json"

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  if exists then removeFile path else pure ()

getDbConnectionString :: IO String
getDbConnectionString = do
  current <- lookupEnv "DB_CONNECTION"
  case current of
    Just value | not (null value) -> return value
    _ -> do
      envfile <- readFile "envfile-test.local"
      let prefix = "DB_CONNECTION=" :: String
      let match = find (prefix `isPrefixOf`) (lines envfile)
      case match of
        Just line -> do
          let value = drop (length prefix) line
          setEnv "DB_CONNECTION" value
          return value
        Nothing -> error "DB_CONNECTION missing from env and envfile-test.local"
