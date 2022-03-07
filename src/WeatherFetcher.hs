{-# LANGUAGE OverloadedStrings #-}

module WeatherFetcher where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forM_)
import           Data.Aeson                 (eitherDecode)
import           Data.Maybe                 (fromJust)
import           Network.HTTP.Simple        (getResponseBody, httpBS,
                                             parseRequest, setRequestHeaders)
import           System.Environment         (getEnv)
import           System.Logger              (Logger, Output (StdOut), create,
                                             debug, err, info)
import           System.Logger.Class        (Logger, debug)
import           System.Logger.Message      (msg)
import           System.Timeout             (timeout)

import           Types

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Database                   as DB

fetchWeather :: Logger -> IO ()
fetchWeather logger = do
  locations <- DB.getLocations
  forM_ locations $ \location -> do
    weather <- fetchWeatherForLocation logger location
    DB.insertLocationWeather (locationLocationID location) weather
    threadDelay (2 * 1000 * 1000) -- 2 second delay

fetchWeatherForLocation :: Logger -> Location -> IO WeatherFetcherResult
fetchWeatherForLocation logger (Location locationID name latitude longitude created) = do
  appID <- getEnv "OPENWEATHERMAP_APPID"
  let url = "http://api.openweathermap.org/data/2.5/weather?lat=" <> show latitude <> "&lon=" <> show longitude <> "&APPID=" <> appID
  System.Logger.debug logger (msg  $ "Fetching " <> name)
  request <- parseRequest url
  responseBody <- checkResponseBody
      <$> timeout (20 * 1000 * 1000) (C.fromStrict . getResponseBody <$> httpBS request)
  let result = responseBody >>= eitherDecode
  case result of
    Left errorMessage -> error errorMessage
    Right weather     -> return weather

checkResponseBody :: Maybe a -> Either String a
checkResponseBody =
  maybe (Left "Timeout while waiting for weather response") Right
