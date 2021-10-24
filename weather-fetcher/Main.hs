{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.Aeson                     ( eitherDecode )
import           Control.Concurrent             ( threadDelay )
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad                  ( forever
                                                , forM_ )
import           Data.Maybe                     ( fromJust )
import           Network.HTTP                   ( simpleHTTP
                                                , getResponseBody
                                                , Request(..)
                                                , RequestMethod(..)
                                                )
import           Network.URI                    ( parseURI )
import           System.Environment             ( getEnv )
import           System.Log.Raven               ( initRaven
                                                , register
                                                , silentFallback
                                                )
import           System.Log.Raven.Transport.HttpConduit
                                                ( sendRecord )
import           System.Log.Raven.Types         ( SentryLevel(Error)
                                                , SentryRecord(..)
                                                )
import           System.Logger                  ( Output(StdOut)
                                                , Logger
                                                , create
                                                , info
                                                , debug
                                                , err
                                                )
import           System.Logger.Class            ( Logger
                                                , debug
                                                )
import           System.Logger.Message          ( msg )
import           System.Timeout                 ( timeout )

import Types

import qualified Database as DB

main :: IO ()
main = do
  logger <- create StdOut
  forever $ do
    info logger (msg @String "Fetching weather")
    catch (fetchWeather logger) (handleException logger)
    threadDelay (15 * 60 * 1000 * 1000) -- 15 mins

handleException :: Logger -> SomeException -> IO ()
handleException logger exception = do
  err logger (msg $ "An error occured: " <> show exception)
  sentryDSN     <- getEnv "WEATHER_FETCHER_SENTRY_DSN"
  env           <- getEnv "ENVIRONMENT"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  register sentryService
           "weather-fetcher-logger"
           Error
           (show exception)
           (recordUpdate env)

recordUpdate :: String -> SentryRecord -> SentryRecord
recordUpdate env record = record { srEnvironment = Just env }

fetchWeather :: Logger -> IO ()
fetchWeather logger = do
  locations <- DB.getLocations
  forM_ locations $ \location -> do
    fetchWeatherForLocation logger location
    threadDelay (2 * 1000 * 1000) -- 2 second delay

fetchWeatherForLocation :: Logger -> Location -> IO ()
fetchWeatherForLocation logger (Location locationID name latitude longitude created) = do
  appID <- getEnv "OPENWEATHERMAP_APPID"
  let url = "http://api.openweathermap.org/data/2.5/weather?lat=" <> show latitude <> "&lon=" <> show longitude <> "&APPID=" <> appID
  System.Logger.debug logger (msg  $ "Fetching " <> name)
  let uri = fromJust . parseURI $ url
  let request = Request uri GET [] ""
  responseBody <- checkResponseBody
      <$> timeout (20 * 1000 * 1000) (simpleHTTP request >>= getResponseBody) -- 20 second timeout
  let result = responseBody >>= eitherDecode
  case result of 
    Left errorMessage -> error errorMessage
    Right weather -> DB.insertLocationWeather locationID weather

checkResponseBody :: Maybe a -> Either String a
checkResponseBody =
  maybe (Left "Timeout while waiting for weather response") Right
