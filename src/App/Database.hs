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
import Data.String (fromString)
import Database.PostgreSQL.Simple
  ( Connection,
    close,
    connectPostgreSQL,
  )

createConnectionPool :: String -> IO (Pool Connection)
createConnectionPool connectionString =
  newPool $
    setNumStripes (Just 2) $
      defaultPoolConfig
        (connectPostgreSQL $ fromString connectionString)
        close
        60 -- unused connections are kept open for a minute
        10 -- max. 10 connections open per stripe
