module PushSpec where

import AWS (EndpointAttributesResult (..), SendNotificationResult (..))
import Control.Monad (forM_, when)
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime (UTCTime))
import Data.Time.Calendar (fromGregorian)
import Data.UUID (fromString)
import Push
  ( PushEndpointClient (..),
    registerDeviceTokenWithClient,
    sendServiceNotificationsWithCleanup,
    shouldNotifyForServiceStatusChange,
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Types
  ( DeviceType (Android, IOS),
    Installation (..),
    Service (..),
    ServiceStatus (..),
  )
import Types.Push
  ( PushPayload (..),
  )

spec :: Spec
spec = do
  describe "Push registration" $ do
    it "creates an endpoint for a new installation" $ do
      actions <- newIORef []
      endpointARN <-
        registerDeviceTokenWithClient
          (recordingClient actions (AttributeResults "new-token" True))
          Nothing
          "new-token"
          Android
      endpointARN `shouldBe` "created:new-token"
      recordedActions <- readIORef actions
      recordedActions
        `shouldBe` [ "create:new-token:Android",
                     "get:created:new-token"
                   ]

    it "creates a replacement endpoint when the stored endpoint no longer exists" $ do
      actions <- newIORef []
      endpointARN <-
        registerDeviceTokenWithClient
          (recordingClient actions EndpointAttributesEndpointNotFound)
          (Just storedInstallation)
          "new-token"
          Android
      endpointARN `shouldBe` "created:new-token"
      recordedActions <- readIORef actions
      recordedActions
        `shouldBe` [ "get:stored-endpoint",
                     "create:new-token:Android"
                   ]

    it "updates an existing endpoint when the token changed" $ do
      actions <- newIORef []
      endpointARN <-
        registerDeviceTokenWithClient
          (recordingClient actions (AttributeResults "old-token" True))
          (Just storedInstallation)
          "new-token"
          Android
      endpointARN `shouldBe` "stored-endpoint"
      recordedActions <- readIORef actions
      recordedActions
        `shouldBe` [ "get:stored-endpoint",
                     "update:stored-endpoint:new-token"
                   ]

    it "updates an existing endpoint when it is disabled" $ do
      actions <- newIORef []
      endpointARN <-
        registerDeviceTokenWithClient
          (recordingClient actions (AttributeResults "new-token" False))
          (Just storedInstallation)
          "new-token"
          Android
      endpointARN `shouldBe` "stored-endpoint"
      recordedActions <- readIORef actions
      recordedActions
        `shouldBe` [ "get:stored-endpoint",
                     "update:stored-endpoint:new-token"
                   ]

    it "leaves an existing enabled endpoint unchanged when the token matches" $ do
      actions <- newIORef []
      endpointARN <-
        registerDeviceTokenWithClient
          (recordingClient actions (AttributeResults "new-token" True))
          (Just storedInstallation)
          "new-token"
          Android
      endpointARN `shouldBe` "stored-endpoint"
      recordedActions <- readIORef actions
      recordedActions `shouldBe` ["get:stored-endpoint"]

  describe "Push notifications" $ do
    it "sends when status changes to Normal, Disrupted, or Cancelled" $
      forM_ notifiableStatuses $ \(status, expectedMessage) -> do
        actions <- newIORef []
        let newService = serviceWithStatus status
        let oldService = serviceWithStatus Unknown
        when (shouldNotifyForServiceStatusChange newService oldService) $
          sendServiceNotificationsWithCleanup
            (recordingClient actions (AttributeResults "token" True))
            (\_ -> pure ())
            newService
            [iosInstallation, androidInstallation]
        recordedActions <- readIORef actions
        recordedActions
          `shouldBe` [ "send:ios-endpoint:" <> expectedMessage,
                       "send:android-endpoint:" <> expectedMessage
                     ]

    it "does not send when the new status is Unknown" $ do
      actions <- newIORef []
      let newService = serviceWithStatus Unknown
      let oldService = serviceWithStatus Normal
      when (shouldNotifyForServiceStatusChange newService oldService) $
        sendServiceNotificationsWithCleanup
          (recordingClient actions (AttributeResults "token" True))
          (\_ -> pure ())
          newService
          [iosInstallation]
      recordedActions <- readIORef actions
      recordedActions `shouldBe` []

    it "does not send when the status is unchanged" $ do
      actions <- newIORef []
      let newService = serviceWithStatus Disrupted
      let oldService = serviceWithStatus Disrupted
      when (shouldNotifyForServiceStatusChange newService oldService) $
        sendServiceNotificationsWithCleanup
          (recordingClient actions (AttributeResults "token" True))
          (\_ -> pure ())
          newService
          [iosInstallation]
      recordedActions <- readIORef actions
      recordedActions `shouldBe` []

    it "deletes the installation and SNS endpoint when the endpoint is disabled" $ do
      actions <- newIORef []
      deletedInstallations <- newIORef []
      sendServiceNotificationsWithCleanup
        (recordingClientWithSendResult actions (AttributeResults "token" True) SendNotificationEndpointDisabled)
        (\installationID -> modifyIORef' deletedInstallations (<> [show installationID]))
        (serviceWithStatus Cancelled)
        [iosInstallation]
      recordedActions <- readIORef actions
      deletedInstallationIDs <- readIORef deletedInstallations
      recordedActions
        `shouldBe` [ "send:ios-endpoint:Sailings have been cancelled for Test Route",
                     "delete:ios-endpoint"
                   ]
      deletedInstallationIDs `shouldBe` [show $ installationID iosInstallation]

recordingClient :: IORef [String] -> EndpointAttributesResult -> PushEndpointClient IO
recordingClient actions endpointAttributesResult =
  recordingClientWithSendResult actions endpointAttributesResult SendNotificationResultSuccess

recordingClientWithSendResult :: IORef [String] -> EndpointAttributesResult -> SendNotificationResult -> PushEndpointClient IO
recordingClientWithSendResult actions endpointAttributesResult sendResult =
  PushEndpointClient
    { pushCreateEndpoint = \deviceToken deviceType -> do
        record $ "create:" <> deviceToken <> ":" <> show deviceType
        pure $ "created:" <> deviceToken,
      pushDeleteEndpoint = \endpointARN ->
        record $ "delete:" <> endpointARN,
      pushGetEndpointAttributes = \endpointARN -> do
        record $ "get:" <> endpointARN
        pure endpointAttributesResult,
      pushUpdateDeviceToken = \endpointARN deviceToken ->
        record $ "update:" <> endpointARN <> ":" <> deviceToken,
      pushSendNotification = \endpointARN payload -> do
        record $ "send:" <> endpointARN <> ":" <> pushPayloadDefault payload
        pure sendResult
    }
  where
    record action = modifyIORef' actions (<> [action])

storedInstallation :: Installation
storedInstallation =
  Installation
    { installationID = fromJust $ fromString "626655d5-855a-4261-b536-b39b9dceddc2",
      installationDeviceToken = "old-token",
      installationDeviceType = Android,
      installationEndpointARN = "stored-endpoint",
      installationPushEnabled = True,
      installationpUpatedDate = UTCTime (fromGregorian 2026 4 28) 0
    }

iosInstallation :: Installation
iosInstallation =
  storedInstallation
    { installationID = fromJust $ fromString "0eb821d7-3c6d-4897-9f25-540ff3479c2d",
      installationDeviceType = IOS,
      installationEndpointARN = "ios-endpoint"
    }

androidInstallation :: Installation
androidInstallation =
  storedInstallation
    { installationID = fromJust $ fromString "73d3948e-1d67-4565-a016-675db1a6b411",
      installationDeviceType = Android,
      installationEndpointARN = "android-endpoint"
    }

serviceWithStatus :: ServiceStatus -> Service
serviceWithStatus status =
  Service
    { serviceID = 42,
      serviceArea = "Test Area",
      serviceRoute = "Test Route",
      serviceStatus = status,
      serviceAdditionalInfo = Nothing,
      serviceDisruptionReason = Nothing,
      serviceOrganisationID = 1,
      serviceLastUpdatedDate = Nothing,
      serviceUpdated = UTCTime (fromGregorian 2026 4 28) 0
    }

notifiableStatuses :: [(ServiceStatus, String)]
notifiableStatuses =
  [ (Normal, "Normal services have resumed for Test Route"),
    (Disrupted, "There is a disruption to the service Test Route"),
    (Cancelled, "Sailings have been cancelled for Test Route")
  ]
