{-# LANGUAGE DeriveGeneric #-}

module Types.Api where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    camelTo2,
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.OpenApi as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (Scientific)
import Data.Time (UTCTime)
import Data.Typeable (Typeable, typeRep)
import GHC.Generics (Generic)
import Types
  ( DeviceType,
    ServiceStatus,
    jsonOptions,
  )

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
    serviceResponseScheduledDeparturesAvailable :: Maybe Bool,
    serviceResponseUpdated :: UTCTime,
    serviceResponseTimetableDocuments :: Maybe [TimetableDocumentResponse]
  }
  deriving (Generic, Show)

instance ToJSON ServiceResponse where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy ServiceResponse)

instance FromJSON ServiceResponse where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy ServiceResponse)

instance OpenApi.ToSchema ServiceResponse where
  declareNamedSchema = OpenApi.genericDeclareNamedSchema $ openApiOptions (Proxy :: Proxy ServiceResponse)

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

instance OpenApi.ToSchema OrganisationResponse where
  declareNamedSchema = OpenApi.genericDeclareNamedSchema $ openApiOptions (Proxy :: Proxy OrganisationResponse)

data TimetableDocumentResponse = TimetableDocumentResponse
  { timetableDocumentResponseID :: Int,
    timetableDocumentResponseOrganisationID :: Int,
    timetableDocumentResponseOrganisationName :: String,
    timetableDocumentResponseServiceIds :: [Int],
    timetableDocumentResponseTitle :: String,
    timetableDocumentResponseSourceURL :: String,
    timetableDocumentResponseContentHash :: Maybe String,
    timetableDocumentResponseContentType :: Maybe String,
    timetableDocumentResponseContentLength :: Maybe Int,
    timetableDocumentResponseLastSeenAt :: UTCTime,
    timetableDocumentResponseUpdated :: UTCTime
  }
  deriving (Generic, Show)

instance FromJSON TimetableDocumentResponse where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy TimetableDocumentResponse)

instance ToJSON TimetableDocumentResponse where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy TimetableDocumentResponse)

instance OpenApi.ToSchema TimetableDocumentResponse where
  declareNamedSchema = OpenApi.genericDeclareNamedSchema $ openApiOptions (Proxy :: Proxy TimetableDocumentResponse)

data CreateInstallationRequest = CreateInstallationRequest
  { createInstallationRequestDeviceToken :: String,
    createInstallationRequestDeviceType :: DeviceType
  }
  deriving (Generic, Show)

instance FromJSON CreateInstallationRequest where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy CreateInstallationRequest)

instance ToJSON CreateInstallationRequest where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy CreateInstallationRequest)

instance OpenApi.ToSchema CreateInstallationRequest where
  declareNamedSchema = OpenApi.genericDeclareNamedSchema $ openApiOptions (Proxy :: Proxy CreateInstallationRequest)

data AddServiceRequest = AddServiceRequest
  { addServiceRequestServiceID :: Int
  }
  deriving (Generic, Show)

instance FromJSON AddServiceRequest where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy AddServiceRequest)

instance ToJSON AddServiceRequest where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy AddServiceRequest)

instance OpenApi.ToSchema AddServiceRequest where
  declareNamedSchema = OpenApi.genericDeclareNamedSchema $ openApiOptions (Proxy :: Proxy AddServiceRequest)

data PushStatus = PushStatus
  { pushStatusEnabled :: Bool
  }
  deriving (Generic, Show)

instance FromJSON PushStatus where
  parseJSON = genericParseJSON $ jsonOptions (Proxy :: Proxy PushStatus)

instance ToJSON PushStatus where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy PushStatus)

instance OpenApi.ToSchema PushStatus where
  declareNamedSchema = OpenApi.genericDeclareNamedSchema $ openApiOptions (Proxy :: Proxy PushStatus)

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

instance OpenApi.ToSchema LocationResponse where
  declareNamedSchema = OpenApi.genericDeclareNamedSchema $ openApiOptions (Proxy :: Proxy LocationResponse)

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

instance OpenApi.ToSchema LocationWeatherResponse where
  declareNamedSchema = OpenApi.genericDeclareNamedSchema $ openApiOptions (Proxy :: Proxy LocationWeatherResponse)

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

instance OpenApi.ToSchema VesselResponse where
  declareNamedSchema = OpenApi.genericDeclareNamedSchema $ openApiOptions (Proxy :: Proxy VesselResponse)

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

instance OpenApi.ToSchema DepartureResponse where
  declareNamedSchema = OpenApi.genericDeclareNamedSchema $ openApiOptions (Proxy :: Proxy DepartureResponse)

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

instance OpenApi.ToSchema RailDepartureResponse where
  declareNamedSchema = OpenApi.genericDeclareNamedSchema $ openApiOptions (Proxy :: Proxy RailDepartureResponse)

openApiOptions :: Typeable a => Proxy a -> OpenApi.SchemaOptions
openApiOptions type' =
  let typeName = show $ typeRep type'
   in OpenApi.defaultSchemaOptions
        { OpenApi.fieldLabelModifier = camelTo2 '_' . drop (length typeName)
        }
