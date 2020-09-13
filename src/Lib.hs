{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
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
import           Data.List                      ( find )
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

import           AWS
import           Types

import qualified Data.ByteString.Lazy.Char8    as C
import qualified Database                      as DB
import qualified Data.Map                      as M

import           Debug.Trace

-- Lookup locations for a service ID
type ServiceLocationLookup = M.Map Int [LocationResponse]

getService :: Int -> Action (Maybe ServiceResponse)
getService serviceID = do
  service        <- DB.getService serviceID
  time           <- liftIO getCurrentTime
  locationLookup <- getLocationLookup
  return $ serviceToServiceResponse locationLookup time <$> service

getServices :: Action [ServiceResponse]
getServices = do
  services       <- DB.getServices
  time           <- liftIO getCurrentTime
  locationLookup <- getLocationLookup
  return $ serviceToServiceResponse locationLookup time <$> services

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
  return $ serviceToServiceResponse locationLookup time <$> services

serviceToServiceResponse
  :: ServiceLocationLookup -> UTCTime -> Service -> ServiceResponse
serviceToServiceResponse locationLookup currentTime Service {..} =
  ServiceResponse
    { serviceResponseServiceID        = serviceID
    , serviceResponseSortOrder        = serviceSortOrder
    , serviceResponseArea             = serviceArea
    , serviceResponseRoute            = serviceRoute
    , serviceResponseStatus           = status
    , serviceResponseLocations        = fromMaybe []
                                          $ M.lookup serviceID locationLookup
    , serviceResponseAdditionalInfo   = serviceAdditionalInfo
    , serviceResponseDisruptionReason = serviceDisruptionReason
    , serviceResponseLastUpdatedDate  = serviceLastUpdatedDate
    , serviceResponseUpdated          = serviceUpdated
    }
 where
  -- Unknown status if over 30 mins ago
  status :: ServiceStatus
  status =
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
  currentEndpointARN <- if (isNothing storedInstallation)
    then liftIO $ createPushEndpoint logger' deviceToken deviceType
    else return $ installationEndpointARN . fromJust $ storedInstallation
  endpointAttributesResult <- liftIO
    $ getAttributesForEndpoint logger' currentEndpointARN
  case endpointAttributesResult of
    EndpointNotFound ->
      liftIO $ createPushEndpoint logger' deviceToken deviceType
    AttributeResults awsDeviceToken isEnabled -> do
      when (awsDeviceToken /= deviceToken || isEnabled == False)
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
            $ \Installation { installationEndpointARN = endpointARN } -> do
                let applePayload = applePushPayloadWithMessageAndServiceID
                      message
                      (serviceID service)
                sendNotificationWihPayload logger endpointARN applePayload
          forM_ androidInterestedInstallations
            $ \Installation { installationEndpointARN = endpointARN } -> do
                let androidPayload = androidPushPayloadWithMessageAndServiceID
                      message
                      (serviceID service)
                sendNotificationWihPayload logger endpointARN androidPayload
      Nothing -> return ()
   where
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
      let apsPayload    = (APSPayload (APSPayloadBody message) serviceID)
          stringPayload = C.unpack . encode $ apsPayload
      in  PushPayload { pushPayloadDefault     = message
                      , pushPayloadApns        = Just stringPayload
                      , pushPayloadApnsSandbox = Just stringPayload
                      , pushPayloadGcm         = Nothing
                      }

    androidPushPayloadWithMessageAndServiceID :: String -> Int -> PushPayload
    androidPushPayloadWithMessageAndServiceID message serviceID =
      let gcmPayload =
              (CGMPayload (GCMPaylodNotification message)
                          (GCMPayloadData serviceID)
              )
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

getLocationLookup :: Action ServiceLocationLookup
getLocationLookup = do
  locations        <- liftIO DB.getLocations
  serviceLocations <- liftIO DB.getServiceLocations
  return $ buildServiceLocationLookup locations serviceLocations

buildServiceLocationLookup
  :: [Location] -> [ServiceLocation] -> ServiceLocationLookup
buildServiceLocationLookup allLocations serviceLocations =
  locationsIDsToLocationResponses <$> serviceIDLocationIDsLookup
 where
  locationsIDsToLocationResponses :: [Int] -> [LocationResponse]
  locationsIDsToLocationResponses locationIDs =
    let locations =
            catMaybes $ findLocationInLocations allLocations <$> locationIDs
    in  locationToLocationResponse <$> locations

  tuplesFromServiceLocations :: [ServiceLocation] -> [(Int, [Int])]
  tuplesFromServiceLocations serviceLocations =
    [ (serviceID, [locationID])
    | (ServiceLocation serviceID locationID) <- serviceLocations
    ]

  serviceIDLocationIDsLookup :: M.Map Int [Int]
  serviceIDLocationIDsLookup =
    M.fromListWith (++) $ tuplesFromServiceLocations serviceLocations

  findLocationInLocations :: [Location] -> Int -> Maybe Location
  findLocationInLocations locations locationIDToFind = find
    (\Location { locationID = locationID } -> locationIDToFind == locationID)
    locations

  locationToLocationResponse :: Location -> LocationResponse
  locationToLocationResponse Location {..} = LocationResponse
    { locationResponseName      = locationName
    , locationResponseLatitude  = locationLatitude
    , locationResponseLongitude = locationLonitude
    }
