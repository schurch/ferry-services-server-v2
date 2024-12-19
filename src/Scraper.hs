{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Scraper
  ( fetchOrkneyFerriesAndNotify,
    fetchShetlandFerriesAndNotify,
    fetchCalMacStatusesAndNotify,
    fetchNorthLinkServicesAndNotify,
    fetchWesternFerriesAndNotify,
  )
where

import AWS
import Amazonka (AWSRequest (response))
import CMark (commonmarkToHtml)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value (String), eitherDecode, encode)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (find, isInfixOf, nub, (\\))
import Data.List.Utils (replace)
import Data.Map (Map, findWithDefault, fromList)
import Data.Maybe (fromJust, fromMaybe)
import Data.Pool (Pool, withResource)
import Data.Text (pack, unpack)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID (UUID)
import qualified Database as DB
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest, setRequestBodyJSON, setRequestHeaders, setRequestMethod)
import Network.HTTP.Types.Header
  ( hAccept,
    hAcceptEncoding,
    hAcceptLanguage,
    hConnection,
    hContentType,
    hHost,
    hOrigin,
    hUserAgent,
  )
import System.Logger.Class (Logger, info)
import System.Logger.Message (msg)
import System.Timeout (timeout)
import Text.HTML.TagSoup (Tag (..), fromAttrib, isTagOpen, isTagOpenName, parseTags, renderTags, (~/=))
import Text.HTML.TagSoup.Tree (renderTree, tagTree)
import Text.Regex (mkRegex, subRegex)
import Types
import Utility (trim)

newtype ScrapedServices = ScrapedServices {unScrapedServices :: [Service]}

newtype DatabaseServices = DatabaseServices {unDatabaseServices :: [Service]}

fetchOrkneyFerriesAndNotify :: Application ()
fetchOrkneyFerriesAndNotify = do
  info (msg @String "Fetching Orkney Ferries services")
  scrapedServices <- liftIO fetchOrkneyFerries
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation 5
  DB.saveServices $ unScrapedServices scrapedServices
  notifyForServices scrapedServices databaseServices

fetchOrkneyFerries :: IO ScrapedServices
fetchOrkneyFerries = do
  htmlTags <- parseTags <$> fetchPage "https://www.orkneyferries.co.uk/info/current-service-update"
  let disruptionTags = takeWhile (~/= ("<a href=/info/about>" :: String)) . dropWhile (~/= ("<h4>" :: String)) $ htmlTags
  let statuses = map (trim . fromAttrib "src") . filter (isTagOpenName "img") $ disruptionTags
  newsTags <- parseTags <$> fetchPage "https://www.orkneyferries.co.uk/news"
  let additionalInfo = "<style>ul>li { margin-bottom: 20px; } ul>li li { margin-bottom: 0px; }</style>" <> (replace "\226\128\147" "-" . replace "\226\128\153" "'" . renderTree . (: []) . head . tagTree . dropWhile (~/= ("<div class=uk-placeholder>" :: String)) $ newsTags)
  time <- liftIO getCurrentTime
  return $
    ScrapedServices
      [ Service
          { serviceID = 4000,
            serviceUpdated = time,
            serviceArea = "Eday",
            serviceRoute = "Kirkwall - Eday - Stronsay - Sanday - Rapness",
            serviceStatus = statusImageTextToStatus $ statuses !! 0,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4001,
            serviceUpdated = time,
            serviceArea = "Sanday",
            serviceRoute = "Kirkwall - Eday - Stronsay - Sanday - Rapness",
            serviceStatus = statusImageTextToStatus $ statuses !! 1,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4002,
            serviceUpdated = time,
            serviceArea = "Stronsay",
            serviceRoute = "Kirkwall - Eday - Stronsay - Sanday - Rapness",
            serviceStatus = statusImageTextToStatus $ statuses !! 2,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4003,
            serviceUpdated = time,
            serviceArea = "Westray",
            serviceRoute = "Kirkwall - Eday - Stronsay - Sanday - Rapness",
            serviceStatus = statusImageTextToStatus $ statuses !! 3,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4004,
            serviceUpdated = time,
            serviceArea = "Shapinsay",
            serviceRoute = "Kirkwall - Shapinsay",
            serviceStatus = statusImageTextToStatus $ statuses !! 4,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4005,
            serviceUpdated = time,
            serviceArea = "Graemsay",
            serviceRoute = "Stromness - Graemsay - Hoy",
            serviceStatus = statusImageTextToStatus $ statuses !! 5,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4006,
            serviceUpdated = time,
            serviceArea = "Houton",
            serviceRoute = "Houton - Flotta - Lyness - Longhope",
            serviceStatus = statusImageTextToStatus $ statuses !! 6,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4007,
            serviceUpdated = time,
            serviceArea = "Rousay, Egilsay & Wyre",
            serviceRoute = "Tingwall - Rousay - Egilsay - Wyre",
            serviceStatus = statusImageTextToStatus $ statuses !! 7,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4008,
            serviceUpdated = time,
            serviceArea = "Pierowall - Papa Westray",
            serviceRoute = "Westray Pierowall - Papa Westray",
            serviceStatus = statusImageTextToStatus $ statuses !! 8,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          }
      ]
  where
    statusImageTextToStatus :: String -> ServiceStatus
    statusImageTextToStatus text
      | "tick" `isInfixOf` text = Normal
      | "warning" `isInfixOf` text = Disrupted
      | "no_entry" `isInfixOf` text = Cancelled
      | otherwise = error $ "Unknown orkney ferries status " <> text

fetchShetlandFerriesAndNotify :: Application ()
fetchShetlandFerriesAndNotify = do
  info (msg @String "Fetching Shetland Ferries services")
  scrapedServices <- liftIO fetchShetlandFerries
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation 4
  DB.saveServices $ unScrapedServices scrapedServices
  notifyForServices scrapedServices databaseServices

fetchShetlandFerries :: IO ScrapedServices
fetchShetlandFerries = do
  htmlTags <- parseTags <$> fetchPage "https://www.shetland.gov.uk/ferrystatus"
  let disruptionTags = takeWhile (~/= ("</div>" :: String)) . dropWhile (~/= ("<div class=routestatus>" :: String)) $ htmlTags
  let statuses = map (trim . fromAttrib "class") . filter (isTagOpenName "ul") $ disruptionTags
  time <- liftIO getCurrentTime
  return $
    ScrapedServices
      [ Service
          { serviceID = 3000,
            serviceUpdated = time,
            serviceArea = "Bluemull Sound",
            serviceRoute = "Gutcher - Belmont - Hamars Ness",
            serviceStatus = statusTextToStatus $ statuses !! 0,
            serviceAdditionalInfo = Just $ createAdditionalInfo "Bluemull Sound" "01595 743971",
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 4,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 3001,
            serviceUpdated = time,
            serviceArea = "Yell",
            serviceRoute = "Toft - Ulsta",
            serviceStatus = statusTextToStatus $ statuses !! 1,
            serviceAdditionalInfo = Just $ createAdditionalInfo "Yell Sound" "01595 743972",
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 4,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 3003,
            serviceUpdated = time,
            serviceArea = "Whalsay",
            serviceRoute = "Laxo - Symbister",
            serviceStatus = statusTextToStatus $ statuses !! 2,
            serviceAdditionalInfo = Just $ createAdditionalInfo "Whalsay" "01595 743973",
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 4,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 3002,
            serviceUpdated = time,
            serviceArea = "Bressay",
            serviceRoute = "Lerwick - Bressay",
            serviceStatus = statusTextToStatus $ statuses !! 3,
            serviceAdditionalInfo = Just $ createAdditionalInfo "Bressay" "01595 743974",
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 4,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 3004,
            serviceUpdated = time,
            serviceArea = "Skerries",
            serviceRoute = "Laxo - Symbister - Skerries - Vidlin - Lerwick",
            serviceStatus = statusTextToStatus $ statuses !! 4,
            serviceAdditionalInfo = Just $ createAdditionalInfo "Skerries" "01595 743975",
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 4,
            serviceLastUpdatedDate = Nothing
          }
      ]
  where
    statusTextToStatus :: String -> ServiceStatus
    statusTextToStatus text
      | text == "Route_status_ok" = Normal
      | text == "Route_status_amber" = Disrupted
      | text == "Route_status_red" = Cancelled
      | otherwise = error $ "Unknown shetland ferries status " <> text

    createAdditionalInfo :: String -> String -> String
    createAdditionalInfo serviceName phoneNumber =
      "For more information on the "
        <> serviceName
        <> " service, phone <a='href=tel:"
        <> phoneNumber
        <> "'>"
        <> phoneNumber
        <> "</a>."
        <> "<p>To subscribe to the Shetland Ferries SMS and email alert system please email "
        <> "<a href='mailto:ferries.admin@shetland.gov.uk?subject=SMS%2FEmail%20Subscription&amp;body=Name%3A%0A%0AAddress%3A%0A%0AMobile%20Number%3A%0A%0AEmail%20Address%3A%0A%0ASubscribed%20Routes%20(e.g%20Bluemull%2C%20Yell%2C%20Whalsay%20etc)%3A%0A%0AEmail%20Alerts%20(Yes%2FNo)%3A%0A%0ASMS%20Alerts%20(Yes%2FNo)%3A'>ferries.admin@shetland.gov.uk</a>"
        <> " with your details</p>"

fetchWesternFerriesAndNotify :: Application ()
fetchWesternFerriesAndNotify = do
  info (msg @String "Fetching Western Ferries service")
  scrapedServices <- ScrapedServices . (: []) <$> fetchWesternFerries
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation 3
  DB.saveServices $ unScrapedServices scrapedServices
  notifyForServices scrapedServices databaseServices

fetchWesternFerries :: Application Service
fetchWesternFerries = do
  htmlTags <- parseTags <$> liftIO (fetchPage "https://status.western-ferries.co.uk/status/view")
  additionalInfo <- liftIO (fetchPage "https://status.western-ferries.co.uk/status/content")
  let activeTag = find (\t -> isTagOpen t && isActiveTag (fromAttrib "class" t)) htmlTags
  let status = textToStatus $ statusClassText <$> activeTag
  time <- liftIO getCurrentTime
  return
    Service
      { serviceID = 2000,
        serviceUpdated = time,
        serviceArea = "Cowal & Dunoon",
        serviceRoute = "McInroy's Point (Gourock) - Hunters Quay (Dunoon)",
        serviceStatus = status,
        serviceAdditionalInfo = if null additionalInfo then Nothing else Just additionalInfo,
        serviceDisruptionReason = Nothing,
        serviceOrganisationID = 3,
        serviceLastUpdatedDate = Nothing
      }
  where
    isActiveTag :: String -> Bool
    isActiveTag = elem "active" . words

    statusClassText :: Tag String -> String
    statusClassText = flip (!!) 1 . words . fromAttrib "class"

    textToStatus :: Maybe String -> ServiceStatus
    textToStatus text
      | text == Just "status-green" = Normal
      | text == Just "status-amber" = Disrupted
      | text == Just "status-red" = Cancelled
      | otherwise = error $ "Unknown western ferries status " <> fromMaybe "" text

fetchNorthLinkServicesAndNotify :: Application ()
fetchNorthLinkServicesAndNotify = do
  info (msg @String "Fetching NorthLink service")
  scrapedServices <- ScrapedServices . (: []) <$> fetchNorthLinkService
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation 2
  DB.saveServices $ unScrapedServices scrapedServices
  notifyForServices scrapedServices databaseServices

fetchNorthLinkService :: Application Service
fetchNorthLinkService = do
  htmlTags <- parseTags <$> liftIO (fetchPage "https://www.northlinkferries.co.uk/opsnews/")
  let statusText = last . words . fromAttrib "class" . head . dropWhile (~/= ("<div id=page>" :: String)) $ htmlTags
  let disruptionInfo = renderTags . takeWhile (~/= ("<!-- .entry-content -->" :: String)) . dropWhile (~/= ("<div class=entry-content>" :: String)) $ htmlTags
  let strippedNewlines = replace "\194\160" "" . replace "\t" "" . replace "\n" ""
  let strippedStyles string = subRegex (mkRegex " style=\"[^\"]*\"") string ""
  time <- liftIO getCurrentTime
  return
    Service
      { serviceID = 1000,
        serviceUpdated = time,
        serviceArea = "Orkney & Shetland",
        serviceRoute = "Scrabster - Stromness / Aberdeen - Kirkwall - Lerwick",
        serviceStatus = textToStatus statusText,
        serviceAdditionalInfo = Just $ strippedNewlines . strippedStyles $ disruptionInfo,
        serviceDisruptionReason = Nothing,
        serviceOrganisationID = 2,
        serviceLastUpdatedDate = Nothing
      }
  where
    textToStatus :: String -> ServiceStatus
    textToStatus text
      | text == "green" = Normal
      | text == "amber" = Disrupted
      | text == "red" = Cancelled
      | otherwise = error $ "Unknown northlink status " <> text

fetchPage :: String -> IO String
fetchPage location = do
  request <- parseRequest location
  response <- timeout (1000000 * 20) (B8.unpack . getResponseBody <$> httpBS request) -- 20 second timeout
  case response of
    Nothing -> error $ "Error fetching " <> location
    Just result -> return result

fetchCalMacStatusesAndNotify :: Application ()
fetchCalMacStatusesAndNotify = do
  info (msg @String "Fetching CalMac services")
  scrapedServices <- liftIO fetchCalMacServices
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation 1
  DB.saveServices $ unScrapedServices scrapedServices
  DB.hideServicesWithIDs $ generateRemovedServiceIDs scrapedServices databaseServices
  notifyForServices scrapedServices databaseServices
  where
    generateRemovedServiceIDs :: ScrapedServices -> DatabaseServices -> [Int]
    generateRemovedServiceIDs (ScrapedServices newServices) (DatabaseServices oldServices) =
      let newServiceIDs = serviceID <$> newServices
          oldServiceIDs = serviceID <$> oldServices
       in nub $ oldServiceIDs \\ newServiceIDs

    fetchCalMacServices :: IO ScrapedServices
    fetchCalMacServices = do
      let headers =
            [ (hContentType, "application/json"),
              (hAcceptEncoding, "gzip, deflate"),
              (hAccept, "*/*"),
              (hUserAgent, "Mozilla/5.0 (iPhone; CPU iPhone OS 18_1_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/15E148"),
              (hHost, "apim.calmac.co.uk"),
              ("Sec-Fetch-Site", "cross-site"),
              (hAcceptLanguage, "en-GB,en;q=0.9"),
              ("Sec-Fetch-Mode", "cors"),
              (hOrigin, "capacitor://localhost"),
              (hConnection, "keep-alive"),
              ("Sec-Fetch-Dest", "empty")
            ]
      let requestBodyQuery = "{\n  routes {\n    name\n    id\n    routeCode\n    ports {\n      portCode\n      name\n      order\n      isFreight\n      hideOnMap\n      __typename\n    }\n    routeStatuses {\n      id\n      title\n      status\n      subStatus\n      title\n      startDateTime\n      endDateTime\n      detail\n      nextReviewDateTime\n      updatedAtDateTime\n      __typename\n    }\n    location {\n      name\n      __typename\n    }\n    status\n    isFreight\n    hideOnMap\n    __typename\n  }\n}"
      request <- setRequestBodyJSON (CalMacAPIRequestBody requestBodyQuery) . setRequestMethod "POST" . setRequestHeaders headers <$> parseRequest "https://apim.calmac.co.uk/graphql"
      responseBody <-
        checkResponseBody
          <$> timeout (1000000 * 20) (C.fromStrict . getResponseBody <$> httpBS request) -- 20 second timeout
      time <- getCurrentTime
      let result = do
            response <- responseBody >>= eitherDecode
            let routes = calMacAPIResponseDataRoutes . calMacAPIResponseData $ response
            Right $ calmacRouteToService time <$> routes
      case result of
        Left errorMessage -> error errorMessage
        Right result' -> return $ ScrapedServices result'

    checkResponseBody :: Maybe a -> Either String a
    checkResponseBody =
      maybe (Left "Timeout while waiting for services response") Right

calmacRouteToService :: UTCTime -> CalMacAPIResponseRoute -> Service
calmacRouteToService time CalMacAPIResponseRoute {..} =
  Service
    { serviceID = findWithDefault (read calMacAPIResponseRouteRouteCode) calMacAPIResponseRouteRouteCode serviceIDLookup,
      serviceUpdated = time,
      serviceArea = calMacAPIResponseRouteLocationName calMacAPIResponseRouteLocation,
      serviceRoute = cleanupRouteText calMacAPIResponseRouteName,
      serviceStatus = statusToServiceStatus calMacAPIResponseRouteStatus,
      serviceAdditionalInfo = Just $ routeStatusesToAdditionalInfo calMacAPIResponseRouteRouteStatuses,
      serviceDisruptionReason = Nothing,
      serviceOrganisationID = 1,
      serviceLastUpdatedDate = Nothing
    }
  where
    statusToServiceStatus :: String -> ServiceStatus
    statusToServiceStatus status
      | status == "NORMAL" = Normal
      | status == "BE_AWARE" = Disrupted
      | status == "DISRUPTIONS" = Disrupted
      | status == "ALL_SAILINGS_CANCELLED" = Cancelled
      | otherwise = error $ "Unknown calmac status " <> status

    cleanupRouteText :: String -> String
    cleanupRouteText = replace "[" "(" . replace "]" ")" . replace "�" "-"

    routeStatusesToAdditionalInfo :: [CalMacAPIResponseRouteStatus] -> String
    routeStatusesToAdditionalInfo statuses = unwords (statusToHTML <$> statuses)

    statusToHTML :: CalMacAPIResponseRouteStatus -> String
    statusToHTML CalMacAPIResponseRouteStatus {..} =
      "<h2>"
        ++ calMacAPIResponseRouteStatusTitle
        ++ "</h2>"
        ++ "<p>"
        ++ unpack (commonmarkToHtml [] (pack calMacAPIResponseRouteStatusDetail))
        ++ "</p>"

    -- Try and map the calmac status code to the old service ids
    serviceIDLookup :: Map String Int
    serviceIDLookup =
      fromList
        [ ("001", 1),
          ("007", 2),
          ("002", 3),
          ("006", 4),
          ("003", 5),
          ("004", 6),
          ("005", 7),
          ("060", 8),
          ("030", 9),
          ("053", 10),
          ("031", 11),
          ("036", 12),
          ("056", 13),
          ("054", 14),
          ("055", 15),
          ("052", 16),
          ("061", 17),
          ("033", 18),
          ("051", 19),
          ("032", 20),
          ("080", 21),
          ("022", 22),
          ("065", 23),
          ("034", 24),
          ("035", 25),
          ("300", 35),
          ("038", 37),
          ("043", 38),
          ("011", 39),
          ("301", 41)
        ]

notifyForServices :: ScrapedServices -> DatabaseServices -> Application ()
notifyForServices (ScrapedServices newServices) (DatabaseServices oldServices) = do
  logger <- asks logger
  forM_ newServices $ \service -> do
    let oldService = find (\s -> serviceID s == serviceID service) oldServices
    case oldService of
      Just oldService -> do
        let statusesDifferent =
              serviceStatus service /= serviceStatus oldService
        let statusValid = serviceStatus service /= Unknown
        let shouldNotify = statusesDifferent && statusValid
        when shouldNotify $ do
          interestedInstallations <- DB.getIntererestedInstallationsForServiceID $ serviceID service
          let defaultNotificationMessage = serviceToDefaultNotificationMessage service
          let iOSInterestedInstallations =
                filter
                  ((==) IOS . installationDeviceType)
                  interestedInstallations
          let (iosTitle, iosBody) = serviceToIOSNotificationMessage service
          forM_ iOSInterestedInstallations $
            \Installation {installationID = installationID, installationEndpointARN = endpointARN} ->
              do
                let payload = createApplePushPayload defaultNotificationMessage iosTitle iosBody (serviceID service)
                sendNotification installationID endpointARN payload

          let androidInterestedInstallations =
                filter
                  ((==) Android . installationDeviceType)
                  interestedInstallations
          let (androidTitle, androidBody) = serviceToAndroidNotificationMessage service
          forM_ androidInterestedInstallations $
            \Installation {installationID = installationID, installationEndpointARN = endpointARN} ->
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
    serviceToDefaultNotificationMessage Service {serviceRoute = serviceRoute, serviceStatus = serviceStatus}
      | serviceStatus == Normal =
          "Normal services have resumed for " <> serviceRoute
      | serviceStatus == Disrupted =
          "There is a disruption to the service " <> serviceRoute
      | serviceStatus == Cancelled =
          "Sailings have been cancelled for " <> serviceRoute
      | serviceStatus == Unknown =
          error "Do not message for unknow service"

    serviceToIOSNotificationMessage :: Service -> (String, String)
    serviceToIOSNotificationMessage Service {serviceArea = serviceArea, serviceRoute = serviceRoute, serviceStatus = serviceStatus}
      | serviceStatus == Normal =
          (serviceArea, "Normal services have resumed for " <> serviceRoute)
      | serviceStatus == Disrupted =
          (serviceArea, "There is a disruption to the service " <> serviceRoute)
      | serviceStatus == Cancelled =
          (serviceArea, "Sailings have been cancelled for " <> serviceRoute)
      | serviceStatus == Unknown =
          error "Do not message for unknow service"

    serviceToAndroidNotificationMessage :: Service -> (String, String)
    serviceToAndroidNotificationMessage Service {serviceArea = serviceArea, serviceRoute = serviceRoute, serviceStatus = serviceStatus}
      | serviceStatus == Normal =
          (serviceArea <> " sailings resumed", serviceRoute)
      | serviceStatus == Disrupted =
          (serviceArea <> " sailings disrupted", serviceRoute)
      | serviceStatus == Cancelled =
          (serviceArea <> " sailings cancelled", serviceRoute)
      | serviceStatus == Unknown =
          error "Do not message for unknow service"

    createApplePushPayload :: String -> String -> String -> Int -> PushPayload
    createApplePushPayload defaultMessage title body serviceID =
      let apsPayload =
            APSPayload
              ( APSPayloadBody
                  { apsPayloadBodyAlert =
                      APSPayloadBodyAlert
                        { apsPayloadBodyAlertTitle = title,
                          apsPayloadBodyAlertBody = body
                        },
                    apsPayloadBodySound = "default"
                  }
              )
              serviceID
       in PushPayload defaultMessage (ApplePayload apsPayload)

    createAndroidPushPayload :: String -> String -> String -> Int -> PushPayload
    createAndroidPushPayload defaultMessage title body serviceID =
      PushPayload defaultMessage (GooglePayload (GCMPayload (GCMPayloadData serviceID title body) "high" (GCMPayloadAndroid "high")))
