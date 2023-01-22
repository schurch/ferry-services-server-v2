{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Scraper where

import           Control.Concurrent         (threadDelay)
import           Control.Exception          (SomeException, catch)
import           Control.Monad              (forM_, forever, void, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (asks)
import           Data.Aeson                 (eitherDecode, encode)
import           Data.List                  (find, nub, (\\))
import           Data.List.Utils            (replace)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Pool                  (Pool, withResource)
import           Data.Time.Clock            (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import           Data.UUID                  (UUID)
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Simple        (getResponseBody, httpBS,
                                             parseRequest, setRequestHeaders)
import           Network.HTTP.Types.Header  (hAccept, hAcceptEncoding,
                                             hContentType, hHost, hUserAgent)
import           System.Logger.Class        (Logger, info)
import           System.Logger.Message      (msg)
import           System.Timeout             (timeout)
import           Text.HTML.TagSoup          (fromAttrib, parseTags, renderTags,
                                             (~/=))
import           Text.Regex                 (mkRegex, subRegex)

import           AWS
import           Types

import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Database                   as DB

newtype ScrapedServices = ScrapedServices { unScrapedServices :: [Service] }
newtype DatabaseServices = DatabaseServices { unDatabaseServices :: [Service] }

fetchNorthLinkServicesAndNotify :: Application ()
fetchNorthLinkServicesAndNotify = do
  info (msg @String "Fetching NorthLink service")
  scrapedServices <- ScrapedServices . (: []) <$> fetchNorthLinkService
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation "NorthLink"
  DB.saveServices $ unScrapedServices scrapedServices
  notifyForServices scrapedServices databaseServices

fetchNorthLinkService :: Application Service
fetchNorthLinkService = do
  htmlTags <- parseTags .  B8.unpack . getResponseBody <$> httpBS "https://www.northlinkferries.co.uk/opsnews/"
  let statusText = last . words . fromAttrib "class" . head . dropWhile (~/= ("<div id=page>" :: String)) $ htmlTags
  let disruptionInfo = renderTags . takeWhile (~/= ("<!-- .entry-content -->" :: String)) . dropWhile (~/= ("<div class=entry-content>" :: String)) $ htmlTags
  let strippedNewlines = replace "\194\160" "" . replace "\t" "" . replace "\n" ""
  let strippedStyles string = subRegex (mkRegex " style=\"[^\"]*\"") string ""
  time <- liftIO getCurrentTime
  return Service
    { serviceID               = 1000
    , serviceUpdated          = time
    , serviceArea             = "ORKNEY & SHETLAND"
    , serviceRoute            = "Scrabster - Stromness / Aberdeen - Kirkwall - Lerwick"
    , serviceStatus           = textToStatus statusText
    , serviceAdditionalInfo   = Just $ strippedNewlines . strippedStyles $ disruptionInfo
    , serviceDisruptionReason = Nothing
    , serviceOrganisation     = "NorthLink"
    , serviceLastUpdatedDate  = Nothing
    }
  where
    textToStatus :: String -> ServiceStatus
    textToStatus text | text == "green"    = Normal
                      | text == "amber"    = Disrupted
                      | text == "red"      = Cancelled
                      | otherwise          = error "Unknown image status"

fetchCalMacStatusesAndNotify :: Application ()
fetchCalMacStatusesAndNotify = do
  info (msg @String "Fetching CalMac services")
  scrapedServices  <- liftIO fetchCalMacServices
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation "CalMac"
  DB.saveServices $ unScrapedServices scrapedServices
  DB.hideServicesWithIDs $ generateRemovedServiceIDs scrapedServices databaseServices
  notifyForServices scrapedServices databaseServices
 where
  generateRemovedServiceIDs :: ScrapedServices -> DatabaseServices -> [Int]
  generateRemovedServiceIDs (ScrapedServices newServices) (DatabaseServices oldServices)
    = let newServiceIDs = serviceID <$> newServices
          oldServiceIDs = serviceID <$> oldServices
      in  nub $ oldServiceIDs \\ newServiceIDs

  fetchCalMacServices :: IO ScrapedServices
  fetchCalMacServices = do
    let headers =
          [ (hContentType, "application/json; charset=utf-8")
          , (hAcceptEncoding, "gzip, deflate")
          , (hAccept, "application/json, text/javascript, */*; q=0.01")
          , (hUserAgent, "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_6) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.1.2 Safari/605.1.15")
          , (hHost, "status.calmac.info")
          ]
    request <- setRequestHeaders headers <$> parseRequest "http://status.calmac.info/?ajax=json"
    responseBody <- checkResponseBody
      <$> timeout (1000000 * 20) (C.fromStrict . getResponseBody <$> httpBS request) -- 20 second timeout
    time <- getCurrentTime
    let result = do
          ajaxResult <- responseBody >>= eitherDecode
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
  , serviceArea             = ajaxServiceDetailsDestName
  , serviceRoute            = ajaxServiceDetailsRouteName
  , serviceStatus           = imageToStatus ajaxServiceDetailsImage
  , serviceAdditionalInfo   = Just
                              $  ajaxServiceDetailsWebDetail
                              <> fromMaybe "" ajaxServiceDetailsInfoMsg
  , serviceDisruptionReason = reasonToMaybe ajaxServiceDetailsReason
  , serviceOrganisation     = "CalMac"
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

notifyForServices :: ScrapedServices -> DatabaseServices -> Application ()
notifyForServices (ScrapedServices newServices) (DatabaseServices oldServices) = do
  logger <- asks logger
  forM_ newServices $ \service -> do
    let oldService = find (\s -> serviceID s == serviceID service) oldServices
    case oldService of
      Just oldService -> do
        let statusesDifferent =
              serviceStatus service /= serviceStatus oldService
        let statusValid  = serviceStatus service /= Unknown
        let shouldNotify = statusesDifferent && statusValid
        when shouldNotify $ do
          interestedInstallations <- DB.getIntererestedInstallationsForServiceID $ serviceID service
          let defaultNotificationMessage = serviceToDefaultNotificationMessage service
          let iOSInterestedInstallations = filter
                ((==) IOS . installationDeviceType)
                interestedInstallations
          forM_ iOSInterestedInstallations
            $ \Installation { installationID = installationID, installationEndpointARN = endpointARN } ->
                do
                  let payload = createApplePushPayload defaultNotificationMessage (serviceID service)
                  sendNotification installationID endpointARN payload

          let androidInterestedInstallations = filter
                ((==) Android . installationDeviceType)
                interestedInstallations
          let (androidTitle, androidBody) = serviceToAndroidNotificationMessage service
          forM_ androidInterestedInstallations
            $ \Installation { installationID = installationID, installationEndpointARN = endpointARN } ->
                do
                  let payload = createAndroidPushPayload defaultNotificationMessage androidTitle androidBody (serviceID service)
                  sendNotification installationID endpointARN payload

      Nothing -> return ()
 where
  sendNotification :: UUID -> String -> PushPayload -> Application ()
  sendNotification installationID endpointARN payload = do
    logger <- asks logger
    result <- liftIO $ sendNotificationWihPayload logger endpointARN payload
    case result of
      SendNotificationEndpointDisabled -> do
        void $ DB.deleteInstallationWithID installationID
        liftIO $ deletePushEndpoint logger endpointARN
      SendNotificationResultSuccess -> return ()

  serviceToDefaultNotificationMessage :: Service -> String
  serviceToDefaultNotificationMessage Service { serviceRoute = serviceRoute, serviceStatus = serviceStatus }
    | serviceStatus == Normal
    = "Normal services have resumed for " <> serviceRoute
    | serviceStatus == Disrupted
    = "There is a disruption to the service " <> serviceRoute
    | serviceStatus == Cancelled
    = "Sailings have been cancelled for " <> serviceRoute
    | serviceStatus == Unknown
    = error "Do not message for unknow service"

  serviceToAndroidNotificationMessage :: Service -> (String, String)
  serviceToAndroidNotificationMessage Service { serviceRoute = serviceRoute, serviceStatus = serviceStatus }
    | serviceStatus == Normal
    = ("Sailings resumed", serviceRoute)
    | serviceStatus == Disrupted
    = ("Sailings disrupted", serviceRoute)
    | serviceStatus == Cancelled
    = ("Sailings cancelled", serviceRoute)
    | serviceStatus == Unknown
    = error "Do not message for unknow service"

  createApplePushPayload :: String -> Int -> PushPayload
  createApplePushPayload message serviceID =
    let apsPayload = APSPayload
          (APSPayloadBody { apsPayloadBodyAlert = message
                          , apsPayloadBodySound = "default"
                          }
          )
          serviceID
    in  PushPayload message (ApplePayload apsPayload)

  createAndroidPushPayload :: String -> String -> String -> Int -> PushPayload
  createAndroidPushPayload defaultMessage title body serviceID =
    PushPayload defaultMessage (GooglePayload (GCMPayload (GCMPayloadData serviceID title body) "high" (GCMPayloadAndroid "high")))
