{-# LANGUAGE RecordWildCards #-}

module Push
  ( PushEndpointClient (..),
    registerDeviceToken,
    registerDeviceTokenWithClient,
    notifyForServices,
  )
where

import AWS
  ( EndpointAttributesResult (..),
    SendNotificationResult (..),
    createPushEndpoint,
    deletePushEndpoint,
    getAttributesForEndpoint,
    sendNotificationWihPayload,
    updateDeviceTokenForEndpoint,
  )
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.List (find)
import Data.UUID (UUID)
import App.Logger (Logger)
import qualified Database as DB
import System.Environment (lookupEnv)
import Types

data PushEndpointClient m = PushEndpointClient
  { pushCreateEndpoint :: String -> DeviceType -> m String,
    pushDeleteEndpoint :: String -> m (),
    pushGetEndpointAttributes :: String -> m EndpointAttributesResult,
    pushUpdateDeviceToken :: String -> String -> m (),
    pushSendNotification :: String -> PushPayload -> m SendNotificationResult
  }

registerDeviceToken :: UUID -> String -> DeviceType -> Application String
registerDeviceToken installationID deviceToken deviceType = do
  logger' <- asks logger
  testEndpointARN <- liftIO $ lookupEnv "FERRY_SERVICES_TEST_AWS_ENDPOINT_ARN"
  case testEndpointARN of
    Just endpointARN -> return endpointARN
    Nothing -> do
      storedInstallation <- DB.getInstallationWithID installationID
      liftIO $
        registerDeviceTokenWithClient
          (awsPushEndpointClient logger')
          storedInstallation
          deviceToken
          deviceType

registerDeviceTokenWithClient ::
  Monad m =>
  PushEndpointClient m ->
  Maybe Installation ->
  String ->
  DeviceType ->
  m String
registerDeviceTokenWithClient client storedInstallation deviceToken deviceType = do
  currentEndpointARN <-
    case storedInstallation of
      Nothing -> pushCreateEndpoint client deviceToken deviceType
      Just installation -> return $ installationEndpointARN installation
  endpointAttributesResult <- pushGetEndpointAttributes client currentEndpointARN
  case endpointAttributesResult of
    EndpointAttributesEndpointNotFound ->
      pushCreateEndpoint client deviceToken deviceType
    AttributeResults awsDeviceToken isEnabled -> do
      when (awsDeviceToken /= deviceToken || not isEnabled) $
        pushUpdateDeviceToken client currentEndpointARN deviceToken
      return currentEndpointARN

notifyForServices :: [Service] -> [Service] -> Application ()
notifyForServices newServices oldServices = do
  logger' <- asks logger
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
          let client = awsPushEndpointClient logger'
          sendServiceNotifications client service interestedInstallations
      Nothing -> return ()

sendServiceNotifications :: PushEndpointClient IO -> Service -> [Installation] -> Application ()
sendServiceNotifications client service interestedInstallations = do
  let defaultNotificationMessage = serviceToDefaultNotificationMessage service
  let iOSInterestedInstallations =
        filter
          ((==) IOS . installationDeviceType)
          interestedInstallations
  let (iosTitle, iosBody) = serviceToIOSNotificationMessage service
  forM_ iOSInterestedInstallations $
    \Installation {installationID = installationID, installationEndpointARN = endpointARN} -> do
      let payload = createApplePushPayload defaultNotificationMessage iosTitle iosBody (serviceID service)
      sendNotification client installationID endpointARN payload

  let androidInterestedInstallations =
        filter
          ((==) Android . installationDeviceType)
          interestedInstallations
  let (androidTitle, androidBody) = serviceToAndroidNotificationMessage service
  forM_ androidInterestedInstallations $
    \Installation {installationID = installationID, installationEndpointARN = endpointARN} -> do
      let payload = createAndroidPushPayload defaultNotificationMessage androidTitle androidBody (serviceID service)
      sendNotification client installationID endpointARN payload

sendNotification :: PushEndpointClient IO -> UUID -> String -> PushPayload -> Application ()
sendNotification client installationID endpointARN payload = do
  result <- liftIO $ pushSendNotification client endpointARN payload
  case result of
    SendNotificationEndpointDisabled -> do
      void $ DB.deleteInstallationWithID installationID
      liftIO $ pushDeleteEndpoint client endpointARN
    SendNotificationResultSuccess -> return ()

awsPushEndpointClient :: Logger -> PushEndpointClient IO
awsPushEndpointClient logger' =
  PushEndpointClient
    { pushCreateEndpoint = createPushEndpoint logger',
      pushDeleteEndpoint = deletePushEndpoint logger',
      pushGetEndpointAttributes = getAttributesForEndpoint logger',
      pushUpdateDeviceToken = updateDeviceTokenForEndpoint logger',
      pushSendNotification = sendNotificationWihPayload logger'
    }

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
