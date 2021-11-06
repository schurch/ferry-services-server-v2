{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Concurrent             ( threadDelay )
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad                  ( forM_, forever )
import           Data.Aeson                     ( eitherDecode )
import           Data.Char                      ( toLower, toUpper )
import           Data.Time.Clock                ( UTCTime (..)
                                                , getCurrentTime
                                                )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Network.HTTP.Simple            ( parseRequest
                                                , getResponseBody
                                                , httpBS
                                                , setRequestHeaders 
                                                )
import           Network.HTTP.Types.Header      ( hAccept
                                                , hAcceptLanguage
                                                , hCookie
                                                , hHost
                                                , hReferer
                                                , hUserAgent 
                                                )      
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
import           System.Logger.Message          ( msg )
import           System.Timeout                 ( timeout )

import Types

import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy.Char8    as C
import qualified Database                      as DB

main :: IO ()
main = do
  logger <- create StdOut
  forever $ do
    info logger (msg @String "Fetching vessels")
    catch (fetchVessels logger) (handleException logger)
    threadDelay (15 * 60 * 1000 * 1000) -- 15 mins

handleException :: Logger -> SomeException -> IO ()
handleException logger exception = do
  err logger (msg $ "An error occured: " <> show exception)
  sentryDSN     <- getEnv "VESSEL_FETCHER_SENTRY_DSN"
  env           <- getEnv "ENVIRONMENT"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  register sentryService
           "vessel-fetcher-logger"
           Error
           (show exception)
           (recordUpdate env)

recordUpdate :: String -> SentryRecord -> SentryRecord
recordUpdate env record = record { srEnvironment = Just env }

fetchVessels :: Logger -> IO ()
fetchVessels logger = do
  forM_ mmsis $ \mmsi -> do
    vessel <- fetchVessel mmsi
    debug logger (msg  $ "Fetched " <> vesselName vessel <> " " <> (show . vesselMmsi $ vessel))
    DB.saveVessel vessel
    threadDelay (4 * 1000 * 1000) -- 4 second delay

fetchVessel :: Int -> IO Vessel
fetchVessel mmsi = do
    let requestParameters = "asset_type=vessels&columns=shipname,mmsi,time_of_latest_position,lat_of_latest_position,lon_of_latest_position,speed,course&mmsi|eq|mmsi=" <> B8.pack (show mmsi)
    let headers = 
            [ (hAccept, "*/*")
            , (hCookie, "SERVERID=app4")
            , (hAcceptLanguage, "en-au")
            , (hHost, "www.marinetraffic.com")
            , (hUserAgent, "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.0 Safari/605.1.15")
            , (hReferer, "https://www.marinetraffic.com/en/data/?" <> requestParameters)
            , ("X-Requested-With", "XMLHttpRequest")
            , ("Vessel-Image", "007fb60815c6558c472a846479502b668e08")
            ]
    request <- setRequestHeaders headers <$> parseRequest (B8.unpack $ "https://www.marinetraffic.com/en/reports?" <> requestParameters)
    responseBody <- checkResponseBody
      <$> timeout (1000000 * 20) (C.fromStrict . getResponseBody <$> httpBS request) -- 20 second timeout
    let result = responseBody >>= eitherDecode
    time <- getCurrentTime
    case result of
      Left  errorMessage -> error errorMessage
      Right result'      -> return $ ajaxToVessel time result'
    where
      ajaxToVessel :: UTCTime -> AjaxVessels -> Vessel
      ajaxToVessel time AjaxVessels {..} = Vessel 
        { vesselMmsi = read . ajaxVesselMmsi . head $ ajaxVesselsData
        , vesselName = capitaliseWords . ajaxVesselShipname . head $ ajaxVesselsData
        , vesselSpeed = read . ajaxVesselSpeed . head $ ajaxVesselsData
        , vesselCourse = read <$> (ajaxVesselCourse . head $ ajaxVesselsData)
        , vesselLatitude = read . ajaxVesselLat . head $ ajaxVesselsData
        , vesselLongitude = read . ajaxVesselLon . head $ ajaxVesselsData
        , vesselLastReceived = toUTC . ajaxVesselLastPos . head $ ajaxVesselsData
        , vesselUpdated = time
        } 
      toUTC :: Int -> UTCTime
      toUTC = posixSecondsToUTCTime . fromInteger . toInteger
      capitaliseWords :: String -> String
      capitaliseWords string = unwords $ toUpperFirstLetter <$> words string
      toUpperFirstLetter :: String -> String
      toUpperFirstLetter [] = []
      toUpperFirstLetter string@(x : xs) = 
        if string == "OF" || string == "THE" then toLower <$> string else toUpper x : (toLower <$> xs)

checkResponseBody :: Maybe a -> Either String a
checkResponseBody =
    maybe (Left "Timeout while waiting for vessel response") Right

mmsis :: [Int]
mmsis = [ 232003244 -- CalMac
        , 235104000
        , 232000420
        , 232003376
        , 232003369
        , 232003371
        , 232003370
        , 232343000
        , 232605000
        , 232003165
        , 232003368
        , 232003372
        , 232001580
        , 232002521
        , 232002598
        , 232003073
        , 232003288
        , 235056506
        , 235000141
        , 235000864
        , 235087611
        , 235008928
        , 235008929
        , 235025112
        , 235052541
        , 235053239
        , 235052285
        , 235083892
        , 235099235
        , 235099237
        , 235101635
        , 235116772
        , 232003166
        , 235449000 -- NorthLink
        , 235450000
        , 235448000
        ]