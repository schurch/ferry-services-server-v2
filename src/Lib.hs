{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( getService
  , getServices
  , createInstallation
  , addServiceToInstallation
  , deleteServiceForInstallation
  , getServicesForInstallation
  , fetchStatusesAndNotify
  , AddServiceRequest(..)
  )
where

import           Control.Monad                  ( void
                                                , when
                                                , forM_
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson                     ( eitherDecode
                                                , encode
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                , fromJust
                                                , catMaybes
                                                )
import           Data.List                      ( find
                                                , sortBy
                                                )
import           Data.Text.Lazy                 ( pack )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                , diffUTCTime
                                                )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Data.UUID                      ( UUID )
import           Network.HTTP                   ( simpleHTTP
                                                , getRequest
                                                , getResponseBody
                                                , Request(..)
                                                , RequestMethod(..)
                                                )
import           System.Logger.Class            ( Logger
                                                , debug
                                                , msg
                                                )
import           Control.Monad.Reader           ( asks )
import           Network.URI                    ( parseURI )
import           Network.HTTP.Headers           ( Header(..)
                                                , HeaderName(..)
                                                )
import           Codec.Compression.GZip         ( decompress )
import           Data.Time.Calendar             ( Day )

import           AWS
import           Types

import qualified Data.ByteString.Lazy.Char8    as C
import qualified Database                      as DB
import qualified Data.Map                      as M

import           Debug.Trace

-- Lookup locations for a service ID
type ServiceLocationLookup = M.Map Int [LocationResponse]

getService :: Int -> Maybe Day -> Action (Maybe ServiceResponse)
getService serviceID timetableDate = do
  service   <- DB.getService serviceID
  time      <- liftIO getCurrentTime
  locations <- getLocationsForServiceID serviceID timetableDate
  return $ serviceToServiceResponseWithLocations locations time <$> service

getServices :: Action [ServiceResponse]
getServices = do
  services       <- DB.getServices
  time           <- liftIO getCurrentTime
  locationLookup <- getLocationLookup
  return
    $   serviceToServiceResponseWithLocationLookup locationLookup time
    <$> services

createInstallation
  :: UUID -> CreateInstallationRequest -> Action [ServiceResponse]
createInstallation installationID (CreateInstallationRequest deviceToken deviceType)
  = do
    awsSNSEndpointARN <- registerDeviceToken installationID
                                             deviceToken
                                             deviceType
    time <- liftIO getCurrentTime
    DB.createInstallation installationID
                          deviceToken
                          deviceType
                          awsSNSEndpointARN
                          time
    getServicesForInstallation installationID

addServiceToInstallation :: UUID -> Int -> Action [ServiceResponse]
addServiceToInstallation installationID serviceID = do
  DB.addServiceToInstallation installationID serviceID
  getServicesForInstallation installationID

deleteServiceForInstallation :: UUID -> Int -> Action [ServiceResponse]
deleteServiceForInstallation installationID serviceID = do
  DB.deleteServiceForInstallation installationID serviceID
  getServicesForInstallation installationID

getServicesForInstallation :: UUID -> Action [ServiceResponse]
getServicesForInstallation installationID = do
  services       <- DB.getServicesForInstallation installationID
  time           <- liftIO getCurrentTime
  locationLookup <- getLocationLookup
  return
    $   serviceToServiceResponseWithLocationLookup locationLookup time
    <$> services

serviceToServiceResponseWithLocations
  :: [LocationResponse] -> UTCTime -> Service -> ServiceResponse
serviceToServiceResponseWithLocations locations currentTime Service {..} =
  ServiceResponse
    { serviceResponseServiceID        = serviceID
    , serviceResponseSortOrder        = serviceSortOrder
    , serviceResponseArea             = serviceArea
    , serviceResponseRoute            = serviceRoute
    , serviceResponseStatus           = serviceStatusForTime currentTime
                                                             serviceUpdated
                                                             serviceStatus
    , serviceResponseLocations        = locations
    , serviceResponseAdditionalInfo   = serviceAdditionalInfo
    , serviceResponseDisruptionReason = serviceDisruptionReason
    , serviceResponseLastUpdatedDate  = serviceLastUpdatedDate
    , serviceResponseUpdated          = serviceUpdated
    }

serviceToServiceResponseWithLocationLookup
  :: ServiceLocationLookup -> UTCTime -> Service -> ServiceResponse
serviceToServiceResponseWithLocationLookup locationLookup currentTime Service {..}
  = ServiceResponse
    { serviceResponseServiceID        = serviceID
    , serviceResponseSortOrder        = serviceSortOrder
    , serviceResponseArea             = serviceArea
    , serviceResponseRoute            = serviceRoute
    , serviceResponseStatus           = serviceStatusForTime currentTime
                                                             serviceUpdated
                                                             serviceStatus
    , serviceResponseLocations        = fromMaybe []
                                          $ M.lookup serviceID locationLookup
    , serviceResponseAdditionalInfo   = serviceAdditionalInfo
    , serviceResponseDisruptionReason = serviceDisruptionReason
    , serviceResponseLastUpdatedDate  = serviceLastUpdatedDate
    , serviceResponseUpdated          = serviceUpdated
    }

-- Unknown status if over 30 mins ago
serviceStatusForTime :: UTCTime -> UTCTime -> ServiceStatus -> ServiceStatus
serviceStatusForTime currentTime serviceUpdated serviceStatus =
  let diff = diffUTCTime currentTime serviceUpdated
  in  if diff > 1800 then Unknown else serviceStatus

-- Pseudo code from AWS docs:
-- retrieve the latest device token from the mobile operating system
-- if (the platform endpoint ARN is not stored)
--   # this is a first-time registration
--   call create platform endpoint
--   store the returned platform endpoint ARN
-- endif

-- call get endpoint attributes on the platform endpoint ARN 

-- if (while getting the attributes a not-found exception is thrown)
--   # the platform endpoint was deleted 
--   call create platform endpoint with the latest device token
--   store the returned platform endpoint ARN
-- else 
--   if (the device token in the endpoint does not match the latest one) or 
--       (get endpoint attributes shows the endpoint as disabled)
--     call set endpoint attributes to set the latest device token and then enable the platform endpoint
--   endif
-- endif
registerDeviceToken :: UUID -> String -> DeviceType -> Action String
registerDeviceToken installationID deviceToken deviceType = do
  logger'            <- asks logger
  storedInstallation <- DB.getInstallationWithID installationID
  currentEndpointARN <- if isNothing storedInstallation
    then liftIO $ createPushEndpoint logger' deviceToken deviceType
    else return $ installationEndpointARN . fromJust $ storedInstallation
  endpointAttributesResult <- liftIO
    $ getAttributesForEndpoint logger' currentEndpointARN
  case endpointAttributesResult of
    EndpointAttributesEndpointNotFound ->
      liftIO $ createPushEndpoint logger' deviceToken deviceType
    AttributeResults awsDeviceToken isEnabled -> do
      when (awsDeviceToken /= deviceToken || not isEnabled)
        $ liftIO
        . void
        $ updateDeviceTokenForEndpoint logger' currentEndpointARN deviceToken
      return currentEndpointARN

fetchStatusesAndNotify :: Logger -> IO ()
fetchStatusesAndNotify logger = do
  services <- fetchServices
  notifyForServices services
  DB.saveServices services
 where
  notifyForServices :: [Service] -> IO ()
  notifyForServices services = forM_ services $ \service -> do
    savedService <- DB.getService $ serviceID service
    case savedService of
      Just savedService -> do
        let statusesDifferent =
              serviceStatus service /= serviceStatus savedService
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
      let apsPayload    = APSPayload (APSPayloadBody message) serviceID
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

  fetchServices :: IO [Service]
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
    responseBody <- simpleHTTP request >>= getResponseBody
    let result = eitherDecode (decompress responseBody)
    case result of
      Left  decodingError         -> error decodingError
      Right serviceDetailsResults -> do
        time <- getCurrentTime
        return $ ajaxResultToService time <$> zip [1 ..] serviceDetailsResults

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
    stringToUTCTime time =
      posixSecondsToUTCTime $ fromInteger (read time) / 1000

getLocationsForServiceID :: Int -> Maybe Day -> Action [LocationResponse]
getLocationsForServiceID serviceID date = do
  serviceLocations <- DB.getLocationsForServiceID serviceID
  case date of
    Just date' -> do
      locationDepartures <- DB.getLocationDeparturesForServiceID serviceID date'
      let departuresLookup = createDeparturesLookup locationDepartures
      return
        $   locationToLocationResponseWithDepartures departuresLookup
        <$> serviceLocations
    Nothing -> return $ locationToLocationResponse <$> serviceLocations
 where
  locationToLocationResponse :: Location -> LocationResponse
  locationToLocationResponse (Location id name latitude longitude) =
    LocationResponse name latitude longitude

  locationToLocationResponseWithDepartures
    :: M.Map Int [DepatureReponse] -> Location -> LocationResponse
  locationToLocationResponseWithDepartures departureLookup (Location id name latitude longitude)
    = let
        departures = M.lookup id departureLookup
        sortedDepartures =
          sortBy
              (\a b -> compare (depatureReponseTime a) (depatureReponseTime b))
            <$> departures
      in
        LocationResponse name latitude longitude

  createDeparturesLookup :: [LocationDeparture] -> M.Map Int [DepatureReponse]
  createDeparturesLookup departures =
    M.fromListWith (++)
      $ [ ( locationID
          , [ DepatureReponse
                (LocationResponse destinationLocationName
                                  destinationLocationLatitude
                                  destinationLocationLatitudeLongitude
                )
                departureTime
                (runtimeToSeconds departureDuration)
            ]
          )
        | (LocationDeparture locationID destinationLocationID destinationLocationName destinationLocationLatitude destinationLocationLatitudeLongitude departureTime departureDuration) <-
          departures
        ]

  runtimeToSeconds :: String -> Int
  runtimeToSeconds runtime =
    read . drop 2 . take (length runtime - 1) $ runtime

getLocationLookup :: Action ServiceLocationLookup
getLocationLookup = do
  serviceLocations <- liftIO DB.getServiceLocations
  return
    $ M.fromListWith (++)
    $ [ (serviceID, [LocationResponse name latitude longitude])
      | (ServiceLocation serviceID locationID name latitude longitude) <-
        serviceLocations
      ]
