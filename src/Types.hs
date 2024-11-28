{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Options (fieldLabelModifier, omitNothingFields),
    ToJSON (toJSON),
    Value (..),
    camelTo2,
    defaultOptions,
    encode,
    genericParseJSON,
    genericToJSON,
    object,
    withScientific,
  )
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (toLower, toUpper)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Proxy
import Data.Scientific
  ( Scientific,
    toBoundedInteger,
  )
import Data.Text.Lazy (Text)
import Data.Time (LocalTime, TimeOfDay, UTCTime)
import Data.Typeable (Typeable, typeRep)
import Data.UUID (UUID)
import Database.Postgis
  ( Geometry,
    readGeometry,
    writeGeometry,
  )
import Database.PostgreSQL.Simple
  ( Connection,
    FromRow,
    ToRow,
  )
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import System.Logger (Logger, log)
import System.Logger.Class (MonadLogger, log)
import Web.Scotty.Trans (ActionT, ScottyT)

data Env = Env
  { logger :: Logger,
    connectionPool :: Pool Connection
  }

type Application = ReaderT Env IO

instance MonadLogger Application where
  log level message = do
    logger <- asks logger
    System.Logger.log logger level message

-- Web server
type Scotty = ScottyT Text Application ()

type Action = ActionT Text Application

instance MonadLogger Types.Action where
  log level message = do
    logger <- asks logger
    System.Logger.log logger level message

-- Push payloads
-- {
--   "default": "This is the default message which must be present when publishing a message to a topic. The default message will only be used if a message is not present for
-- one of the notification platforms.",
--   "APNS": "{\"aps\":{\"alert\": \"Check out these awesome deals!\",\"url\":\"www.amazon.com\"} }",
--   "GCM": "{\"data\":{\"message\":\"Check out these awesome deals!\",\"url\":\"www.amazon.com\"}}",
--   "ADM": "{\"data\":{\"message\":\"Check out these awesome deals!\",\"url\":\"www.amazon.com\"}}"
-- }
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

-- Apple
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

-- Google
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

-- General
data ServiceStatus = Normal | Disrupted | Cancelled | Unknown deriving (Show, Eq)

instance Enum ServiceStatus where
  toEnum 0 = Normal
  toEnum 1 = Disrupted
  toEnum 2 = Cancelled
  toEnum _ = Unknown

  fromEnum Normal = 0
  fromEnum Disrupted = 1
  fromEnum Cancelled = 2
  fromEnum Unknown = -99

instance ToJSON ServiceStatus where
  toJSON = toJSON . fromEnum

instance FromJSON ServiceStatus where
  parseJSON = withScientific "ServiceStatus" (pure . toEnum . fromMaybe (-99) . toBoundedInteger)

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
instance ToField Geometry where
  toField = toField . writeGeometry

instance FromField Geometry where
  fromField f m = case m of
    Just bs -> return $ readGeometry . B.fromStrict $ bs
    Nothing -> error "Invalid Field"

data Service = Service
  { serviceID :: Int,
    serviceArea :: String,
    serviceRoute :: String,
    serviceStatus :: ServiceStatus,
    serviceAdditionalInfo :: Maybe String,
    serviceDisruptionReason :: Maybe String,
    serviceOrganisationID :: Int,
    serviceLastUpdatedDate :: Maybe UTCTime,
    serviceUpdated :: UTCTime
  }
  deriving (Generic, Show, ToRow, FromRow)

data Installation = Installation
  { installationID :: UUID,
    installationDeviceToken :: String,
    installationDeviceType :: DeviceType,
    installationEndpointARN :: String,
    installationPushEnabled :: Bool,
    installationpUpatedDate :: UTCTime
  }
  deriving (Generic, Show, ToRow, FromRow)

data ServiceLocation = ServiceLocation
  { serviceLocationServiceID :: Int,
    serviceLocationLocationID :: Int,
    serviceLocationName :: String,
    serviceLocationCoordinate :: Geometry
  }
  deriving (Generic, Show, ToRow, FromRow)

data Location = Location
  { locationLocationID :: Int,
    locationName :: String,
    locationCoordinate :: Geometry,
    locationCreated :: UTCTime
  }
  deriving (Generic, Show, ToRow, FromRow)

data LocationWeather = LocationWeather
  { locationWeatherLocationID :: Int,
    locationWeatherDescription :: String,
    locationWeatherIcon :: String,
    locationWeatherTemperature :: Scientific,
    locationWeatherWindSpeed :: Scientific,
    locationWeatherWindDirection :: Scientific,
    locationWeatherUpdated :: UTCTime,
    locationWeatherCreated :: UTCTime
  }
  deriving (Generic, Show, ToRow, FromRow)

data Vessel = Vessel
  { vesselMmsi :: Int,
    vesselName :: String,
    vesselSpeed :: Maybe Scientific,
    vesselCourse :: Maybe Scientific,
    vesselCoordinate :: Geometry,
    vesselLastReceived :: UTCTime,
    vesselUpdated :: UTCTime,
    vesselOrganisationID :: Int
  }
  deriving (Generic, Show, ToRow, FromRow)

data ServiceVessel = ServiceVessel
  { serviceVesselSeviceID :: Int,
    serviceVesselMmsi :: Int,
    serviceVesselName :: String,
    serviceVesselSpeed :: Maybe Scientific,
    serviceVesselCourse :: Maybe Scientific,
    serviceVesselCoordinate :: Geometry,
    serviceVesselLastReceived :: UTCTime,
    serviceVesselUpdated :: UTCTime,
    serviceVesselOrganisationID :: Int
  }
  deriving (Generic, Show, ToRow, FromRow)

data LocationDeparture = LocationDeparture
  { locationDepartureFromLocationID :: Int,
    locationDepartureToLocationID :: Int,
    locationDepartureToLocationName :: String,
    locationDepartureToLocationCoordinate :: Geometry,
    locationDepartureDepartue :: LocalTime,
    locationDepartureArrival :: LocalTime,
    locationDepartureNotes :: Maybe String
  }
  deriving (Generic, Show, ToRow, FromRow)

data ServiceOrganisation = ServiceOrganisation
  { serviceOrganisationServiceID :: Int,
    serviceOrganisationOrganisationID :: Int,
    serviceOrganisationName :: String,
    serviceOrganisationWebsite :: Maybe String,
    serviceOrganisationLocalPhone :: Maybe String,
    serviceOrganisationInternationalPhone :: Maybe String,
    serviceOrganisationEmail :: Maybe String,
    serviceOrganisationX :: Maybe String,
    serviceOrganisationFacebook :: Maybe String
  }
  deriving (Generic, Show, ToRow, FromRow)

data LocationRailDeparture = LocationRailDeparture
  { locationRailDepartureFromLocationID :: Int,
    locationRailDepartureDepartureCRS :: String,
    locationRailDepartureDepartureName :: String,
    locationRailDepartureDestinationCRS :: String,
    locationRailDepartureDestinationName :: String,
    locationRailDepartureScheduledDepartureTime :: LocalTime,
    locationRailDepartureEstimatedDepartureTime :: String,
    locationRailDepartureCancelled :: Bool,
    locationRailDeparturePlatform :: Maybe String
  }
  deriving (Generic, Show, ToRow, FromRow)

-- API Types
data ServiceResponse = ServiceResponse
  { serviceResponseServiceID :: Int,
    serviceResponseSortOrder :: Int,
    serviceResponseArea :: String,
    serviceResponseRoute :: String,
    serviceResponseStatus :: ServiceStatus,
    serviceResponseLocations :: [LocationResponse],
    serviceResponseAdditionalInfo :: Maybe String,
    serviceResponseDisruptionReason :: Maybe String,
    serviceResponseLastUpdatedDate :: Maybe UTCTime,
    serviceResponseVessels :: [VesselResponse],
    serviceResponseOperator :: Maybe OrganisationResponse,
    serviceResponseUpdated :: UTCTime
  }
  deriving (Generic, Show)

instance ToJSON ServiceResponse where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy ServiceResponse)

instance FromJSON ServiceResponse where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy ServiceResponse)

data OrganisationResponse = OrganisationResponse
  { organisationResponseID :: Int,
    organisationResponseName :: String,
    organisationResponseWebsite :: Maybe String,
    organisationResponseLocalNumber :: Maybe String,
    organisationResponseInternationalNumber :: Maybe String,
    organisationResponseEmail :: Maybe String,
    organisationResponseX :: Maybe String,
    organisationResponseFacebook :: Maybe String
  }
  deriving (Generic, Show)

instance FromJSON OrganisationResponse where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy OrganisationResponse)

instance ToJSON OrganisationResponse where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy OrganisationResponse)

data CreateInstallationRequest = CreateInstallationRequest
  { createInstallationRequestDeviceToken :: String,
    createInstallationRequestDeviceType :: DeviceType
  }
  deriving (Generic, Show)

instance FromJSON CreateInstallationRequest where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy CreateInstallationRequest)

instance ToJSON CreateInstallationRequest where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy CreateInstallationRequest)

data AddServiceRequest = AddServiceRequest
  { addServiceRequestServiceID :: Int
  }
  deriving (Generic, Show)

instance FromJSON AddServiceRequest where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy AddServiceRequest)

instance ToJSON AddServiceRequest where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy AddServiceRequest)

data PushStatus = PushStatus
  { pushStatusEnabled :: Bool
  }
  deriving (Generic, Show)

instance FromJSON PushStatus where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy PushStatus)

instance ToJSON PushStatus where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy PushStatus)

data LocationResponse = LocationResponse
  { locationResponseID :: Int,
    locationResponseName :: String,
    locationResponseLatitude :: Scientific,
    locationResponseLongitude :: Scientific,
    locationResponseScheduledDepartures :: Maybe [DepartureResponse],
    locationResponseNextDeparture :: Maybe DepartureResponse,
    locationResponseNextRailDeparture :: Maybe RailDepartureResponse,
    locationResponseWeather :: Maybe LocationWeatherResponse
  }
  deriving (Generic, Show)

instance ToJSON LocationResponse where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy LocationResponse)

instance FromJSON LocationResponse where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy LocationResponse)

data LocationWeatherResponse = LocationWeatherResponse
  { locationWeatherResponseIcon :: String,
    locationWeatherResponseDescription :: String,
    locationWeatherResponseTemperatureCelsius :: Int,
    locationWeatherResponseWindSpeedMPH :: Int,
    locationWeatherResponseWindDirection :: Scientific,
    locationWeatherResponseWindDirectionCardinal :: String
  }
  deriving (Generic, Show)

instance ToJSON LocationWeatherResponse where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy LocationWeatherResponse)

instance FromJSON LocationWeatherResponse where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy LocationWeatherResponse)

data VesselResponse = VesselResponse
  { vesselResponseMmsi :: Int,
    vesselResponseName :: String,
    vesselResponseSpeed :: Maybe Scientific,
    vesselResponseCourse :: Maybe Scientific,
    vesselResponseLatitude :: Scientific,
    vesselResponseLongitude :: Scientific,
    vesselResponseLastReceived :: UTCTime
  }
  deriving (Generic, Show)

instance ToJSON VesselResponse where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy VesselResponse)

instance FromJSON VesselResponse where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy VesselResponse)

data DepartureResponse = DepartureResponse
  { departureResponseDestination :: LocationResponse,
    departureResponseDeparture :: UTCTime,
    departureResponseArrival :: UTCTime,
    departureResponseNotes :: Maybe String
  }
  deriving (Generic, Show)

instance ToJSON DepartureResponse where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy DepartureResponse)

instance FromJSON DepartureResponse where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy DepartureResponse)

data RailDepartureResponse = RailDepartureResponse
  { railDepartureResponseFrom :: String,
    railDepartureResponseTo :: String,
    railDepartureResponseDeparture :: UTCTime,
    railDepartureResponseDepartureInfo :: String,
    railDepartureResponsePlatform :: Maybe String,
    railDepartureResponseIsCancelled :: Bool
  }
  deriving (Generic, Show)

instance ToJSON RailDepartureResponse where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy RailDepartureResponse)

instance FromJSON RailDepartureResponse where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy RailDepartureResponse)

jsonOptions :: Typeable a => Proxy a -> Data.Aeson.Options
jsonOptions type' =
  let typeName = show $ typeRep type'
   in defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (length typeName),
          omitNothingFields = True
        }

-- Scraper Types
data CalMacAPIRequestBody = CalMacAPIRequestBody
  { calMacAPIRequestBodyQuery :: String
  }
  deriving (Generic, Show)

instance ToJSON CalMacAPIRequestBody where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy CalMacAPIRequestBody)

data CalMacAPIResponse = CalMacAPIResponse
  { calMacAPIResponseData :: CalMacAPIResponseData
  }
  deriving (Generic, Show)

instance FromJSON CalMacAPIResponse where
  parseJSON = genericParseJSON $ calMacAPIResponseJsonOptions (Proxy :: Proxy CalMacAPIResponse)

data CalMacAPIResponseData = CalMacAPIResponseData
  { calMacAPIResponseDataRoutes :: [CalMacAPIResponseRoute]
  }
  deriving (Generic, Show)

instance FromJSON CalMacAPIResponseData where
  parseJSON = genericParseJSON $ calMacAPIResponseJsonOptions (Proxy :: Proxy CalMacAPIResponseData)

data CalMacAPIResponseRoute = CalMacAPIResponseRoute
  { calMacAPIResponseRouteName :: String,
    calMacAPIResponseRouteStatus :: String,
    calMacAPIResponseRouteRouteCode :: String,
    calMacAPIResponseRouteLocation :: CalMacAPIResponseRouteLocation
  }
  deriving (Generic, Show)

instance FromJSON CalMacAPIResponseRoute where
  parseJSON = genericParseJSON $ calMacAPIResponseJsonOptions (Proxy :: Proxy CalMacAPIResponseRoute)

data CalMacAPIResponseRouteLocation = CalMacAPIResponseRouteLocation
  { calMacAPIResponseRouteLocationName :: String
  }
  deriving (Generic, Show)

instance FromJSON CalMacAPIResponseRouteLocation where
  parseJSON = genericParseJSON $ calMacAPIResponseJsonOptions (Proxy :: Proxy CalMacAPIResponseRouteLocation)

calMacAPIResponseJsonOptions :: Typeable a => Proxy a -> Data.Aeson.Options
calMacAPIResponseJsonOptions type' =
  let typeName = show $ typeRep type'
   in defaultOptions
        { fieldLabelModifier = toLowerFirstLetter . drop (length typeName),
          omitNothingFields = True
        }

data AjaxServiceDetails = AjaxServiceDetails
  { ajaxServiceDetailsReason :: String,
    ajaxServiceDetailsImage :: String,
    ajaxServiceDetailsDestName :: String,
    ajaxServiceDetailsCode :: String,
    ajaxServiceDetailsInfoIncluded :: String,
    ajaxServiceDetailsInfoMsg :: Maybe String,
    ajaxServiceDetailsReported :: String,
    ajaxServiceDetailsId :: String,
    ajaxServiceDetailsWebDetail :: String,
    ajaxServiceDetailsUpdated :: String,
    ajaxServiceDetailsRouteName :: String,
    ajaxServiceDetailsStatus :: String
  }
  deriving (Generic, Show)

instance FromJSON AjaxServiceDetails where
  parseJSON =
    genericParseJSON $
      defaultOptions
        { fieldLabelModifier = toLowerFirstLetter . drop (length . show . typeRep $ (Proxy :: Proxy AjaxServiceDetails))
        }

-- Weather Fetcher Types
data WeatherFetcherResult = WeatherFetcherResult
  { weatherFetcherResultWeather :: [WeatherFetcherResultWeather],
    weatherFetcherResultMain :: WeatherFetcherResultMain,
    weatherFetcherResultWind :: WeatherFetcherResultWind
  }
  deriving (Generic, Show)

instance FromJSON WeatherFetcherResult where
  parseJSON = genericParseJSON $ weatherFetcherJsonOptions (Proxy :: Proxy WeatherFetcherResult)

data WeatherFetcherResultWeather = WeatherFetcherResultWeather
  { weatherFetcherResultWeatherIcon :: String,
    weatherFetcherResultWeatherDescription :: String
  }
  deriving (Generic, Show)

instance FromJSON WeatherFetcherResultWeather where
  parseJSON = genericParseJSON $ weatherFetcherJsonOptions (Proxy :: Proxy WeatherFetcherResultWeather)

data WeatherFetcherResultMain = WeatherFetcherResultMain
  { weatherFetcherResultMainTemp :: Scientific
  }
  deriving (Generic, Show)

instance FromJSON WeatherFetcherResultMain where
  parseJSON = genericParseJSON $ weatherFetcherJsonOptions (Proxy :: Proxy WeatherFetcherResultMain)

data WeatherFetcherResultWind = WeatherFetcherResultWind
  { weatherFetcherResultWindSpeed :: Scientific,
    weatherFetcherResultWindDeg :: Scientific
  }
  deriving (Generic, Show)

instance FromJSON WeatherFetcherResultWind where
  parseJSON = genericParseJSON $ weatherFetcherJsonOptions (Proxy :: Proxy WeatherFetcherResultWind)

weatherFetcherJsonOptions :: Typeable a => Proxy a -> Options
weatherFetcherJsonOptions type' =
  let typeName = show $ typeRep type'
   in defaultOptions {fieldLabelModifier = camelTo2 '_' . drop (length typeName)}

-- Vessel Fetcher Types
data AjaxVessel = AjaxVessel
  { ajaxVesselMmsi :: String,
    ajaxVesselShipname :: String,
    ajaxVesselLat :: String,
    ajaxVesselLon :: String,
    ajaxVesselSpeed :: Maybe String,
    ajaxVesselCourse :: Maybe String,
    ajaxVesselTimestamp :: String
  }
  deriving (Generic, Show)

instance FromJSON AjaxVessel where
  parseJSON =
    genericParseJSON $
      defaultOptions
        { fieldLabelModifier = map toUpper . drop (length . show . typeRep $ (Proxy :: Proxy AjaxVessel))
        }

-- Rail Departure Fetcher Types
data RailDepartureFetcherResult = RailDepartureFetcherResult
  { railDepartureFetcherResultLocationName :: String,
    railDepartureFetcherResultCrs :: String,
    railDepartureFetcherResultTrainServices :: Maybe [RailDepartureFetcherTrainService]
  }
  deriving (Generic, Show)

instance FromJSON RailDepartureFetcherResult where
  parseJSON = genericParseJSON $ railDepartureFetcherJsonOptions (Proxy :: Proxy RailDepartureFetcherResult)

data RailDepartureFetcherTrainService = RailDepartureFetcherTrainService
  { railDepartureFetcherTrainServiceDestination :: [RailDepartureFetcherLocation],
    railDepartureFetcherTrainServiceCurrentDestinations :: Maybe [RailDepartureFetcherLocation],
    railDepartureFetcherTrainServiceStd :: String,
    railDepartureFetcherTrainServiceEtd :: String,
    railDepartureFetcherTrainServicePlatform :: Maybe String,
    railDepartureFetcherTrainServiceIsCancelled :: Bool
  }
  deriving (Generic, Show)

instance FromJSON RailDepartureFetcherTrainService where
  parseJSON = genericParseJSON $ railDepartureFetcherJsonOptions (Proxy :: Proxy RailDepartureFetcherTrainService)

data RailDepartureFetcherLocation = RailDepartureFetcherLocation
  { railDepartureFetcherLocationLocationName :: String,
    railDepartureFetcherLocationCrs :: String
  }
  deriving (Generic, Show)

instance FromJSON RailDepartureFetcherLocation where
  parseJSON = genericParseJSON $ railDepartureFetcherJsonOptions (Proxy :: Proxy RailDepartureFetcherLocation)

railDepartureFetcherJsonOptions :: Typeable a => Proxy a -> Options
railDepartureFetcherJsonOptions type' =
  let typeName = show $ typeRep type'
   in defaultOptions {fieldLabelModifier = toLowerFirstLetter . drop (length typeName)}

-- Helpers
toLowerFirstLetter :: String -> String
toLowerFirstLetter [] = []
toLowerFirstLetter (x : xs) = toLower x : xs
