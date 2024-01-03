{-# LANGUAGE OverloadedStrings #-}

module RailDepartureFetcher where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.UTF8 as BSU (fromString)
import qualified Database as DB
import Network.HTTP.Simple
  ( getResponseBody,
    httpBS,
    parseRequest,
    setRequestHeaders,
  )
import System.Environment (getEnv)
import System.Logger.Class (debug, err)
import System.Logger.Message (msg)
import System.Timeout (timeout)
import Types (Application, RailDepartureFetcherResult (RailDepartureFetcherResult), railDepartureFetcherJsonOptions)

fetchRailDepartures :: Application ()
fetchRailDepartures =
  forM_
    [ ("ADS", 3),
      ("LAR", 11),
      ("WMS", 7),
      ("GRK", 56),
      ("OBN", 19),
      ("MLG", 44),
      ("THS", 59),
      ("ABD", 61)
    ]
    fetchRailDeparture

fetchRailDeparture :: (String, Int) -> Application ()
fetchRailDeparture (crs, locationID) = do
  debug (msg $ "Fetching " <> crs)
  apiKey <- liftIO $ getEnv "RAIL_DATA_API_KEY"
  let headers = [("x-apikey", BSU.fromString apiKey)]
  request <- setRequestHeaders headers <$> parseRequest ("https://api1.raildata.org.uk/1010-live-departure-board-dep/LDBWS/api/20220120/GetDepBoardWithDetails/" <> crs)
  responseBody <-
    liftIO $
      checkResponseBody
        <$> timeout (20 * 1000 * 1000) (C.fromStrict . getResponseBody <$> httpBS request)
  let result = responseBody >>= eitherDecode
  case result of
    Left errorMessage -> error $ "Error fetching " <> crs <> ":" <> errorMessage
    Right railDepartures -> DB.insertRailDepartureFetcherResult railDepartures locationID

checkResponseBody :: Maybe a -> Either String a
checkResponseBody =
  maybe (Left "Timeout while waiting for rail departures response") Right
