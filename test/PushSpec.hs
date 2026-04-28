module PushSpec where

import AWS (EndpointAttributesResult (..), SendNotificationResult (SendNotificationResultSuccess))
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
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Types
  ( DeviceType (Android),
    Installation (..),
  )

spec :: Spec
spec =
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

recordingClient :: IORef [String] -> EndpointAttributesResult -> PushEndpointClient IO
recordingClient actions endpointAttributesResult =
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
      pushSendNotification = \endpointARN _ -> do
        record $ "send:" <> endpointARN
        pure SendNotificationResultSuccess
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
