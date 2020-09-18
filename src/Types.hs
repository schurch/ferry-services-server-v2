{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import           Control.Monad.Reader           ( ReaderT
                                                , asks
                                                )
import           Data.Aeson
import           Data.Char                      ( toLower )
import           Data.Scientific                ( Scientific )
import           Data.Text.Lazy                 ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple
import           System.Logger                  ( Logger
                                                , log
                                                )
import           System.Logger.Class            ( MonadLogger
                                                , log
                                                )
import           Web.Scotty.Trans               ( ScottyT
                                                , ActionT
                                                )
import           GHC.Generics

-- Web server
data Env = Env { logger :: Logger }

type Scotty = ScottyT Text (ReaderT Env IO) ()
type Action = ActionT Text (ReaderT Env IO)

instance MonadLogger Types.Action where
  log level message = do
    logger <- asks logger
    System.Logger.log logger level message

-- Push payloads
-- Apple
data APSPayload = APSPayload {
    apsPayloadAps :: APSPayloadBody
  , apsPayloadServiceID :: Int
} deriving (Generic, Show)

instance ToJSON APSPayload where
  toJSON = genericToJSON $ jsonOptions 10

data APSPayloadBody = APSPayloadBody {
    apsPayloadBodyAlert :: String
} deriving (Generic, Show)

instance ToJSON APSPayloadBody where
  toJSON = genericToJSON $ jsonOptions 14

-- Google
data CGMPayload = CGMPayload {
    gcmPayloadNotification :: GCMPaylodNotification
  , gcmPayloadData :: GCMPayloadData
} deriving (Generic, Show)

instance ToJSON CGMPayload where
  toJSON = genericToJSON $ jsonOptions 10

data GCMPaylodNotification = GCMPaylodNotification {
  gcmPaylodNotificationTitle :: String
} deriving (Generic, Show)

instance ToJSON GCMPaylodNotification where
  toJSON = genericToJSON $ jsonOptions 21

data GCMPayloadData = GCMPayloadData {
  gcmPayloadDataServiceID :: Int
} deriving (Generic, Show)

instance ToJSON GCMPayloadData where
  toJSON = genericToJSON $ jsonOptions 14

-- General
data ServiceStatus = Normal | Disrupted | Cancelled | Unknown deriving (Show, Eq)

instance Enum ServiceStatus where
  toEnum 0     = Normal
  toEnum 1     = Disrupted
  toEnum 2     = Cancelled
  toEnum (-99) = Unknown

  fromEnum Normal    = 0
  fromEnum Disrupted = 1
  fromEnum Cancelled = 2
  fromEnum Unknown   = -99

instance ToJSON ServiceStatus where
  toJSON = toJSON . fromEnum

instance ToField ServiceStatus where
  toField = toField . fromEnum

instance FromField ServiceStatus where
  fromField field byteString = toEnum <$> fromField field byteString


data DeviceType = IOS | Android deriving (Eq, Show, Generic, Bounded, Enum)

instance ToJSON DeviceType

instance FromJSON DeviceType

instance ToField DeviceType where
  toField = toField . fromEnum

instance FromField DeviceType where
  fromField field byteString = toEnum <$> fromField field byteString

-- Database Types
data Service = Service {
    serviceID :: Int
  , serviceSortOrder :: Int
  , serviceArea :: String
  , serviceRoute :: String
  , serviceStatus :: ServiceStatus
  , serviceAdditionalInfo :: Maybe String
  , serviceDisruptionReason :: Maybe String
  , serviceLastUpdatedDate :: Maybe UTCTime
  , serviceUpdated :: UTCTime
} deriving (Generic, Show, ToRow, FromRow)

data Installation = Installation {
    installationID :: UUID
  , installationDeviceToken :: String
  , installationDeviceType :: DeviceType
  , installationEndpointARN :: String
  , installationpUpatedDate :: UTCTime
} deriving (Generic, Show, ToRow, FromRow)

data ServiceLocation = ServiceLocation {
    serviceLocationServiceID :: Int
  , serviceLocationLocationID :: Int
  , serviceLocationName :: String
  , serviceLocationLatitude :: Scientific
  , serviceLocationLonitude :: Scientific
} deriving (Generic, Show, ToRow, FromRow)

-- API Types
data ServiceResponse = ServiceResponse {
    serviceResponseServiceID :: Int
  , serviceResponseSortOrder :: Int
  , serviceResponseArea :: String
  , serviceResponseRoute :: String
  , serviceResponseStatus :: ServiceStatus
  , serviceResponseLocations :: [LocationResponse]
  , serviceResponseAdditionalInfo :: Maybe String
  , serviceResponseDisruptionReason :: Maybe String
  , serviceResponseLastUpdatedDate :: Maybe UTCTime
  , serviceResponseUpdated :: UTCTime
} deriving (Generic, Show)

instance ToJSON ServiceResponse where
  toJSON = genericToJSON $ jsonOptions 15

data CreateInstallationRequest = CreateInstallationRequest {
    createInstallationRequestDeviceToken :: String
  , createInstallationRequestDeviceType :: DeviceType
} deriving (Generic, Show)

instance FromJSON CreateInstallationRequest where
  parseJSON = genericParseJSON $ jsonOptions 25

data AddServiceRequest = AddServiceRequest {
    addServiceRequestServiceID :: Int
} deriving (Generic, Show)

instance FromJSON AddServiceRequest where
  parseJSON = genericParseJSON $ jsonOptions 17

data LocationResponse = LocationResponse {
    locationResponseName :: String
  , locationResponseLatitude :: Scientific
  , locationResponseLongitude :: Scientific
} deriving (Generic, Show)

instance ToJSON LocationResponse where
  toJSON = genericToJSON $ jsonOptions 16

jsonOptions :: Int -> Data.Aeson.Options
jsonOptions prefixLength =
  defaultOptions { fieldLabelModifier = camelTo2 '_' . drop prefixLength }

-- Scraper Types
data AjaxServiceDetails = AjaxServiceDetails {
    ajaxServiceDetailsReason :: String
  , ajaxServiceDetailsImage :: String
  , ajaxServiceDetailsDestName :: String
  , ajaxServiceDetailsCode :: String
  , ajaxServiceDetailsInfoIncluded :: String
  , ajaxServiceDetailsInfoMsg :: Maybe String
  , ajaxServiceDetailsReported :: String
  , ajaxServiceDetailsId :: String
  , ajaxServiceDetailsWebDetail :: String
  , ajaxServiceDetailsUpdated :: String
  , ajaxServiceDetailsRouteName :: String
  , ajaxServiceDetailsStatus :: String
} deriving (Generic, Show)

instance FromJSON AjaxServiceDetails where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = toLowerFirstLetter . drop 18 }

toLowerFirstLetter :: String -> String
toLowerFirstLetter []       = []
toLowerFirstLetter (x : xs) = toLower x : xs
