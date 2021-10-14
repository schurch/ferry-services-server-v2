{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Codec.Compression.GZip         ( decompress )
import           Control.Concurrent             ( threadDelay )
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad                  ( void
                                                , when
                                                , forM_
                                                , forever
                                                )
import           Data.Aeson                     ( eitherDecode
                                                , encode
                                                )
import           Data.List                      ( find
                                                , nub
                                                , (\\)
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , fromJust
                                                )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Data.UUID                      ( UUID )
import           Network.HTTP                   ( simpleHTTP
                                                , getResponseBody
                                                , Request(..)
                                                , RequestMethod(..)
                                                )
import           Network.HTTP.Headers           ( Header(..)
                                                , HeaderName(..)
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

import           Types
import           AWS

import qualified Data.ByteString.Lazy.Char8    as C
import qualified Database                      as DB

main :: IO ()
main = do
  logger <- create StdOut
  forever $ do
    info logger (msg @String "Fetching statuses...")
    catch (fetchStatusesAndNotify logger) (handleException logger)
    threadDelay (15 * 60 * 1000 * 1000) -- 15 mins

handleException :: Logger -> SomeException -> IO ()
handleException logger exception = do
  err logger (msg $ "An error occured: " <> show exception)
  sentryDSN     <- getEnv "SCRAPER_SENTRY_DSN"
  env           <- getEnv "ENVIRONMENT"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  register sentryService
           "scraper-logger"
           Error
           (show exception)
           (recordUpdate env)

recordUpdate :: String -> SentryRecord -> SentryRecord
recordUpdate env record = record { srEnvironment = Just env }

newtype ScrapedServices = ScrapedServices { unScrapedServices :: [Service] }
newtype DatabaseServices = DatabaseServices { unDatabaseServices :: [Service] }

fetchStatusesAndNotify :: Logger -> IO ()
fetchStatusesAndNotify logger = do
  scrapedServices  <- fetchServices
  databaseServices <- DatabaseServices <$> DB.getServices
  DB.saveServices $ unScrapedServices scrapedServices
  DB.hideServicesWithIDs
    $ generateRemovedServiceIDs scrapedServices databaseServices
  notifyForServices logger scrapedServices databaseServices
 where
  generateRemovedServiceIDs :: ScrapedServices -> DatabaseServices -> [Int]
  generateRemovedServiceIDs (ScrapedServices newServices) (DatabaseServices oldServices)
    = let newServiceIDs = serviceID <$> newServices
          oldServiceIDs = serviceID <$> oldServices
      in  nub $ oldServiceIDs \\ newServiceIDs

  fetchServices :: IO ScrapedServices
  fetchServices = do
    let uri = fromJust $ parseURI "http://status.calmac.info/?ajax=json"
    let
      request = Request
        uri
        GET
        [ Header HdrContentType "application/json; charset=utf-8"
        , Header HdrAcceptEncoding "gzip, deflate"
        , Header HdrAccept "application/json, text/javascript, */*; q=0.01"
        , Header
          HdrUserAgent
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_6) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.1.2 Safari/605.1.15"
        , Header HdrHost "status.calmac.info"
        ]
        ""
    responseBody <- checkResponseBody
      <$> timeout (1000000 * 20) (simpleHTTP request >>= getResponseBody) -- 20 second timeout
    time <- getCurrentTime
    let result = do
          ajaxResult <- responseBody >>= eitherDecode . decompress
          Right $ ajaxResultToService time <$> zip [1 ..] ajaxResult
    case result of
      Left  errorMessage -> error errorMessage
      Right result'      -> return $ ScrapedServices result'

  checkResponseBody :: Maybe a -> Either String a
  checkResponseBody =
    maybe (Left "Timeout while waiting for services response") Right

ajaxResultToService :: UTCTime -> (Int, AjaxServiceDetails) -> Service
ajaxResultToService time (sortOrder, AjaxServiceDetails {..}) = Service
  { serviceID               = read ajaxServiceDetailsCode
  , serviceUpdated          = time
  , serviceSortOrder        = sortOrder
  , serviceArea             = ajaxServiceDetailsDestName
  , serviceRoute            = ajaxServiceDetailsRouteName
  , serviceStatus           = imageToStatus ajaxServiceDetailsImage
  , serviceAdditionalInfo   = Just
                              $  ajaxServiceDetailsWebDetail
                              <> fromMaybe "" ajaxServiceDetailsInfoMsg
  , serviceDisruptionReason = reasonToMaybe ajaxServiceDetailsReason
  , serviceLastUpdatedDate  = Just $ stringToUTCTime ajaxServiceDetailsUpdated
  }
 where
  reasonToMaybe :: String -> Maybe String
  reasonToMaybe reason | reason == "NONE" = Nothing
                       | otherwise        = Just reason

  imageToStatus :: String -> ServiceStatus
  imageToStatus image | image == "normal"    = Normal
                      | image == "beware"    = Disrupted
                      | image == "affected"  = Disrupted
                      | image == "cancelled" = Cancelled
                      | otherwise            = error "Unknown image status"

  stringToUTCTime :: String -> UTCTime
  stringToUTCTime time = posixSecondsToUTCTime $ fromInteger (read time) / 1000

notifyForServices :: Logger -> ScrapedServices -> DatabaseServices -> IO ()
notifyForServices logger (ScrapedServices newServices) (DatabaseServices oldServices)
  = forM_ newServices $ \service -> do
    let oldService = find (\s -> serviceID s == serviceID service) oldServices
    case oldService of
      Just oldService -> do
        let statusesDifferent =
              serviceStatus service /= serviceStatus oldService
        let statusValid  = serviceStatus service /= Unknown
        let shouldNotify = statusesDifferent && statusValid
        when shouldNotify $ do
          let message = serviceToNotificationMessage service
          interestedInstallations <- DB.getIntererestedInstallationsForServiceID
            $ serviceID service
          let iOSInterestedInstallations = filter
                ((==) IOS . installationDeviceType)
                interestedInstallations
          let androidInterestedInstallations = filter
                ((==) Android . installationDeviceType)
                interestedInstallations
          forM_ iOSInterestedInstallations
            $ \Installation { installationID = installationID, installationEndpointARN = endpointARN } ->
                do
                  let
                    payload = applePushPayloadWithMessageAndServiceID
                      message
                      (serviceID service)
                  sendNotification logger installationID endpointARN payload
          forM_ androidInterestedInstallations
            $ \Installation { installationID = installationID, installationEndpointARN = endpointARN } ->
                do
                  let
                    payload = androidPushPayloadWithMessageAndServiceID
                      message
                      (serviceID service)
                  sendNotification logger installationID endpointARN payload
      Nothing -> return ()
 where
  sendNotification :: Logger -> UUID -> String -> PushPayload -> IO ()
  sendNotification logger installationID endpointARN payload = do
    result <- sendNotificationWihPayload logger endpointARN payload
    case result of
      SendNotificationEndpointDisabled -> do
        void $ DB.deleteInstallationWithID installationID
        deletePushEndpoint logger endpointARN
      SendNotificationResultSuccess -> return ()

  serviceToNotificationMessage :: Service -> String
  serviceToNotificationMessage Service { serviceRoute = serviceRoute, serviceStatus = serviceStatus }
    | serviceStatus == Normal
    = "Normal services have resumed for " <> serviceRoute
    | serviceStatus == Disrupted
    = "There is a disruption to the service " <> serviceRoute
    | serviceStatus == Cancelled
    = "Sailings have been cancelled for " <> serviceRoute
    | serviceStatus == Unknown
    = error "Do not message for unknow service"

  applePushPayloadWithMessageAndServiceID :: String -> Int -> PushPayload
  applePushPayloadWithMessageAndServiceID message serviceID =
    let apsPayload = APSPayload
          (APSPayloadBody { apsPayloadBodyAlert = message
                          , apsPayloadBodySound = "default"
                          }
          )
          serviceID
        stringPayload = C.unpack . encode $ apsPayload
    in  PushPayload { pushPayloadDefault     = message
                    , pushPayloadApns        = Just stringPayload
                    , pushPayloadApnsSandbox = Just stringPayload
                    , pushPayloadGcm         = Nothing
                    }

  androidPushPayloadWithMessageAndServiceID :: String -> Int -> PushPayload
  androidPushPayloadWithMessageAndServiceID message serviceID =
    let gcmPayload = CGMPayload (GCMPaylodNotification message)
                                (GCMPayloadData serviceID)
        stringPayload = C.unpack . encode $ gcmPayload
    in  PushPayload { pushPayloadDefault     = message
                    , pushPayloadApns        = Nothing
                    , pushPayloadApnsSandbox = Nothing
                    , pushPayloadGcm         = Just stringPayload
                    }
