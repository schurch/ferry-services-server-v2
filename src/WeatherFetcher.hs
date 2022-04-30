{-# LANGUAGE OverloadedStrings #-}

module WeatherFetcher where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forM_)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (asks)
import           Data.Aeson                 (eitherDecode)
import           Data.Maybe                 (fromJust)
import           Data.Pool                  (Pool, withResource)
import           Database.Postgis           (Geometry (GeoPoint), Point (Point),
                                             Position (Position))
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Simple        (getResponseBody, httpBS,
                                             parseRequest, setRequestHeaders)
import           System.Environment         (getEnv)
import           System.Logger.Class        (debug)
import           System.Logger.Message      (msg)
import           System.Timeout             (timeout)

import           Types

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Database                   as DB

fetchWeather :: Application ()
fetchWeather = do
  locations <-  DB.getLocations
  fetchWeatherForLocations locations

fetchWeatherForLocations :: [Location] -> Application ()
fetchWeatherForLocations locations = do
  forM_ locations $ \location -> do
    weather <- fetchWeatherForLocation location
    DB.insertLocationWeather (locationLocationID location) weather
    liftIO $ threadDelay (2 * 1000 * 1000) -- 2 second delay

fetchWeatherForLocation :: Location -> Application WeatherFetcherResult
fetchWeatherForLocation (Location locationID name (GeoPoint _ (Point (Position latitude longitude _ _))) created) = do
  appID <- liftIO $ getEnv "OPENWEATHERMAP_APPID"
  let url = "http://api.openweathermap.org/data/2.5/weather?lat=" <> show latitude <> "&lon=" <> show longitude <> "&APPID=" <> appID
  debug (msg  $ "Fetching " <> name)
  request <- parseRequest url
  responseBody <- liftIO $ checkResponseBody
      <$> timeout (20 * 1000 * 1000) (C.fromStrict . getResponseBody <$> httpBS request)
  let result = responseBody >>= eitherDecode
  case result of
    Left errorMessage -> error errorMessage
    Right weather     -> return weather
fetchWeatherForLocation _ = error "Expected point"

checkResponseBody :: Maybe a -> Either String a
checkResponseBody =
  maybe (Left "Timeout while waiting for weather response") Right
