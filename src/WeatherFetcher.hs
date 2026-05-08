{-# LANGUAGE OverloadedStrings #-}

module WeatherFetcher where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Database as DB
import App.Env (Application)
import Network.HTTP.Simple
  ( getResponseBody,
    getResponseStatusCode,
    parseRequest,
    httpBS,
  )
import System.Environment (lookupEnv)
import App.Logger (logDebugM, logErrorM)
import System.Timeout (timeout)
import Types
import Types.Weather

fetchWeather :: Application ()
fetchWeather = do
  appID <- liftIO $ lookupEnv "OPENWEATHERMAP_APPID"
  case appID of
    Nothing -> logErrorM "OPENWEATHERMAP_APPID is not set; skipping weather fetch"
    Just "" -> logErrorM "OPENWEATHERMAP_APPID is empty; skipping weather fetch"
    Just configuredAppID -> do
      locations <- DB.getLocations
      fetchWeatherForLocations configuredAppID locations

fetchWeatherForLocations :: String -> [Location] -> Application ()
fetchWeatherForLocations appID locations = do
  forM_ locations $ \location -> do
    maybeWeather <- fetchWeatherForLocation appID location
    case maybeWeather of
      Nothing -> return ()
      Just weather -> DB.insertLocationWeather (locationLocationID location) weather
    liftIO $ threadDelay (2 * 1000 * 1000) -- 2 second delay

fetchWeatherForLocation :: String -> Location -> Application (Maybe WeatherFetcherResult)
fetchWeatherForLocation appID (Location locationID name (Coordinate latitude longitude) _created) = do
  let url = "http://api.openweathermap.org/data/2.5/weather?lat=" <> show latitude <> "&lon=" <> show longitude <> "&APPID=" <> appID
  logDebugM $ "Fetching " <> name
  request <- parseRequest url
  responseResult <-
    liftIO $
      checkResponse
        <$> timeout (20 * 1000 * 1000) (httpBS request)
  case responseResult of
    Left errorMessage -> do
      logErrorM $ "Skipping weather for " <> name <> " (" <> show locationID <> "): " <> errorMessage
      return Nothing
    Right response -> do
      let statusCode = getResponseStatusCode response
          responseBody = C.fromStrict $ getResponseBody response
      if statusCode < 200 || statusCode >= 300
        then do
          logErrorM $ "Skipping weather for " <> name <> " (" <> show locationID <> "): OpenWeather returned HTTP " <> show statusCode <> " - " <> C.unpack (C.take 500 responseBody)
          return Nothing
        else case eitherDecode responseBody of
          Left errorMessage -> do
            logErrorM $ "Skipping weather for " <> name <> " (" <> show locationID <> "): could not parse OpenWeather response - " <> errorMessage <> " - " <> C.unpack (C.take 500 responseBody)
            return Nothing
          Right weather -> return $ Just weather

checkResponse :: Maybe a -> Either String a
checkResponse =
  maybe (Left "Timeout while waiting for weather response") Right
