{-# LANGUAGE OverloadedStrings #-}

module App.Database
  ( createConnectionPool,
  )
where

import Data.Pool
  ( Pool,
    defaultPoolConfig,
    newPool,
    setNumStripes,
  )
import Database.SQLite.Simple
  ( Connection,
    close,
    execute_,
    open,
  )

createConnectionPool :: String -> IO (Pool Connection)
createConnectionPool databasePath =
  newPool $
    setNumStripes (Just 1) $
      defaultPoolConfig
        (openConfigured databasePath)
        close
        60 -- unused connections are kept open for a minute
        4 -- SQLite benefits from a small pool; writes are still serialized

openConfigured :: FilePath -> IO Connection
openConfigured databasePath = do
  connection <- open databasePath
  execute_ connection "PRAGMA foreign_keys = ON"
  execute_ connection "PRAGMA journal_mode = WAL"
  execute_ connection "PRAGMA synchronous = NORMAL"
  execute_ connection "PRAGMA busy_timeout = 5000"
  pure connection
