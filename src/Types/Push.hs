{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Push where

import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    Value (..),
    encode,
    genericToJSON,
    object,
  )
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Types (jsonOptions)

data PushPayload = PushPayload
  { pushPayloadDefault :: String,
    pushPayloadContent :: PushPayloadContent
  }
  deriving (Show)

data PushPayloadContent = ApplePayload APSPayload | GooglePayload GCMPayload deriving (Show)

instance ToJSON PushPayload where
  toJSON (PushPayload text (ApplePayload apns)) =
    object
      [ "default" .= text,
        "APNS" .= (C.unpack . encode $ apns),
        "APNS_SANDBOX" .= (C.unpack . encode $ apns),
        "GCM" .= Null
      ]
  toJSON (PushPayload text (GooglePayload gcm)) =
    object
      [ "default" .= text,
        "APNS" .= Null,
        "APNS_SANDBOX" .= Null,
        "GCM" .= (C.unpack . encode $ gcm)
      ]

data APSPayload = APSPayload
  { apsPayloadAps :: APSPayloadBody,
    apsPayloadServiceID :: Int
  }
  deriving (Generic, Show)

instance ToJSON APSPayload where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy APSPayload)

data APSPayloadBody = APSPayloadBody
  { apsPayloadBodyAlert :: APSPayloadBodyAlert,
    apsPayloadBodySound :: String
  }
  deriving (Generic, Show)

instance ToJSON APSPayloadBody where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy APSPayloadBody)

data APSPayloadBodyAlert = APSPayloadBodyAlert
  { apsPayloadBodyAlertTitle :: String,
    apsPayloadBodyAlertBody :: String
  }
  deriving (Generic, Show)

instance ToJSON APSPayloadBodyAlert where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy APSPayloadBodyAlert)

data GCMPayload = GCMPayload
  { gcmPayloadData :: GCMPayloadData,
    gcmPayloadPriority :: String,
    gcmPayloadAndroid :: GCMPayloadAndroid
  }
  deriving (Generic, Show)

instance ToJSON GCMPayload where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy GCMPayload)

data GCMPayloadData = GCMPayloadData
  { gcmPayloadDataServiceID :: Int,
    gcmPayloadDataTitle :: String,
    gcmPayloadDataBody :: String
  }
  deriving (Generic, Show)

instance ToJSON GCMPayloadData where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy GCMPayloadData)

data GCMPayloadAndroid = GCMPayloadAndroid
  { gcmPayloadAndroidPriority :: String
  }
  deriving (Generic, Show)

instance ToJSON GCMPayloadAndroid where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy GCMPayloadAndroid)
