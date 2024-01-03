{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VesselFetcher where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (toLower, toUpper)
import Data.Pool (Pool, withResource)
import Data.Time
  ( UTCTime,
    defaultTimeLocale,
    getCurrentTime,
    parseTimeOrError,
  )
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Database as DB
import Database.Postgis
  ( Geometry (GeoPoint),
    Point (Point),
    Position (Position),
  )
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Simple
  ( getResponseBody,
    httpBS,
    parseRequest,
    setRequestHeaders,
  )
import Network.HTTP.Types.Header
  ( hAccept,
    hAcceptLanguage,
    hCookie,
    hHost,
    hReferer,
    hUserAgent,
  )
import System.Environment (getEnv)
import System.Logger.Class (debug, err)
import System.Logger.Message (msg)
import System.Timeout (timeout)
import Types

type OrganisationID = Int

type MMSI = Int

fetchVessels :: [(OrganisationID, [MMSI])] -> Application ()
fetchVessels organisations = do
  forM_ organisations $ \(organisationID, mmsis) -> do
    forM_ mmsis $ \mmsi -> do
      vessel <- liftIO $ fetchVessel organisationID mmsi
      case vessel of
        Left error ->
          err (msg $ "Error fetching " <> show mmsi)
        Right vessel -> do
          debug (msg $ "Fetched " <> vesselName vessel <> " " <> (show . vesselMmsi $ vessel))
          DB.saveVessel vessel
      liftIO $ threadDelay (4 * 1000 * 1000) -- 4 second delay

fetchVessel :: OrganisationID -> MMSI -> IO (Either String Vessel)
fetchVessel organisationID mmsi = do
  let headers =
        [ (hAccept, "application/json, text/plain, */*"),
          (hUserAgent, "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.0 Safari/605.1.15"),
          (hAcceptLanguage, "en-US,en;q=0.5"),
          ("X-NewRelic-ID", "undefined"),
          ("Vessel-Image", "0054729193319b27a6d45397a3b5a4d83e17"),
          ("X-Requested-With", "XMLHttpRequest"),
          ("Alt-Used", "www.marinetraffic.com"),
          ("Connection", "keep-alive"),
          (hReferer, "https://www.marinetraffic.com/en/ais/home/centerx:-5.5/centery:56.4/zoom:8"),
          ("Sec-Fetch-Dest", "empty"),
          ("Sec-Fetch-Mode", "cors"),
          ("Sec-Fetch-Site", "same-origin")
        ]
  request <- setRequestHeaders headers <$> parseRequest ("https://www.marinetraffic.com/map/getvesseljson/mmsi:" <> show mmsi)
  responseBody <-
    checkResponseBody
      <$> timeout (1000000 * 20) (C.fromStrict . getResponseBody <$> httpBS request) -- 20 second timeout
  let result = responseBody >>= eitherDecode
  time <- getCurrentTime
  return $ ajaxToVessel time <$> result
  where
    ajaxToVessel :: UTCTime -> AjaxVessel -> Vessel
    ajaxToVessel time AjaxVessel {..} =
      let latitude = read ajaxVesselLat :: Double
          longitude = read ajaxVesselLon :: Double
       in Vessel
            { vesselMmsi = read ajaxVesselMmsi,
              vesselName = capitaliseWords ajaxVesselShipname,
              vesselSpeed = read <$> ajaxVesselSpeed,
              vesselCourse = read <$> ajaxVesselCourse,
              vesselCoordinate = GeoPoint (Just 4326) (Point (Position latitude longitude Nothing Nothing)),
              vesselLastReceived = toUTC ajaxVesselTimestamp,
              vesselUpdated = time,
              vesselOrganisationID = organisationID
            }
    toUTC :: String -> UTCTime
    toUTC = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
    capitaliseWords :: String -> String
    capitaliseWords string = unwords $ toUpperFirstLetter <$> words string
    toUpperFirstLetter :: String -> String
    toUpperFirstLetter [] = []
    toUpperFirstLetter string@(x : xs) =
      if string == "OF" || string == "THE" then toLower <$> string else toUpper x : (toLower <$> xs)

checkResponseBody :: Maybe a -> Either String a
checkResponseBody =
  maybe (Left "Timeout while waiting for vessel response") Right

defaultMmsis :: [(OrganisationID, [MMSI])]
defaultMmsis =
  [ ( 1, -- CalMac
      [ 232003244,
        235104000,
        232000420,
        232003376,
        232003369,
        232003371,
        232003370,
        232343000,
        232605000,
        232003165,
        232003368,
        232003372,
        232001580,
        232002521,
        232002598,
        232003073,
        232003288,
        235056506,
        235000141,
        235000864,
        235087611,
        235008928,
        235008929,
        235025112,
        235052541,
        235053239,
        235052285,
        235083892,
        235099235,
        235099237,
        235101635,
        235116772,
        232003166,
        232019501
      ]
    ),
    ( 2, -- NorthLink
      [ 235449000,
        235450000,
        235448000
      ]
    ),
    ( 3, -- Western Ferries
      [ 235001902,
        235013197,
        235101062,
        235101063
      ]
    ),
    ( 4, -- Shetland Ferries
      [ 232003606,
        232003604,
        232003597,
        232003605,
        235009928,
        232003608,
        232003598,
        235003893,
        235014766,
        235014768,
        232003596,
        232003607
      ]
    ),
    ( 5, -- Orkney Ferries
      [ 232000670,
        232000760,
        232000550,
        235019175,
        235018907,
        235021681,
        235019173,
        235019174,
        235018919,
        232029607,
        235021681
      ]
    )
  ]
