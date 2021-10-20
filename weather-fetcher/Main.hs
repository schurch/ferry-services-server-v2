{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.Aeson                     ( eitherDecode )
import           Control.Concurrent             ( threadDelay )
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad                  ( forever )
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
                                                , err
                                                )
import           System.Logger.Class            ( Logger
                                                , debug
                                                )
import           System.Logger.Message          ( msg )
import           System.Timeout                 ( timeout )

import Types

main :: IO ()
main = do
  logger <- create StdOut
  forever $ do
    info logger (msg @String "Fetching weather...")
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
  appID <- getEnv "OPENWEATHERMAP_APPID"
  let uri = fromJust $ parseURI $ "http://api.openweathermap.org/data/2.5/weather?lat=55.576606&lon=-5.139172&APPID=" <> appID
  let request = Request uri GET [] ""
  responseBody <- checkResponseBody
      <$> timeout (1000000 * 20) (simpleHTTP request >>= getResponseBody) -- 20 second timeout
  let result = do
        ajaxResult <- responseBody >>= eitherDecode
        Right $ resultToDatabase ajaxResult
  print result

resultToDatabase :: WeatherFetcherResult -> WeatherFetcherResult
resultToDatabase result = result

checkResponseBody :: Maybe a -> Either String a
checkResponseBody =
  maybe (Left "Timeout while waiting for services response") Right
