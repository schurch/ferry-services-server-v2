{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier, omitNothingFields),
    ToJSON (toJSON),
    Value (Number, String),
    camelTo2,
    defaultOptions,
    withScientific,
  )
import Control.Lens ((&), (?~))
import Data.Maybe (fromMaybe)
import qualified Data.OpenApi as OpenApi
import Data.Proxy
import Data.Scientific
  ( Scientific,
    fromFloatDigits,
    toBoundedInteger,
    toRealFloat,
  )
import Data.Time (LocalTime, TimeOfDay, UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Typeable (Typeable, typeRep)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.SQLite.Simple.FromField (Field, FieldParser, FromField (..), ResultError (ConversionFailed), returnError)
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple.ToRow (ToRow)
import GHC.Generics (Generic)
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

instance OpenApi.ToSchema ServiceStatus where
  declareNamedSchema _ =
    pure $
      OpenApi.NamedSchema (Just "ServiceStatus") $
        mempty
          & OpenApi.type_ ?~ OpenApi.OpenApiInteger
          & OpenApi.enum_ ?~ [Number 0, Number 1, Number 2, Number (-99)]

instance ToField ServiceStatus where
  toField = toField . fromEnum

instance FromField ServiceStatus where
  fromField field = toEnum <$> fromField field

data DeviceType = IOS | Android deriving (Eq, Show, Generic, Bounded, Enum)

instance ToJSON DeviceType

instance FromJSON DeviceType

instance OpenApi.ToSchema DeviceType where
  declareNamedSchema _ =
    pure $
      OpenApi.NamedSchema (Just "DeviceType") $
        mempty
          & OpenApi.type_ ?~ OpenApi.OpenApiString
          & OpenApi.enum_ ?~ [String "IOS", String "Android"]

instance ToField DeviceType where
  toField = toField . fromEnum

instance FromField DeviceType where
  fromField field = toEnum <$> fromField field

instance ToField UUID where
  toField = toField . UUID.toString

instance FromField UUID where
  fromField field = do
    value <- fromField field
    case UUID.fromString value of
      Just uuid -> pure uuid
      Nothing -> returnError ConversionFailed field "Invalid UUID"

instance ToField Scientific where
  toField = toField . (toRealFloat :: Scientific -> Double)

instance FromField Scientific where
  fromField field = fromFloatDigits <$> (fromField :: FieldParser Double) field

instance ToField TimeOfDay where
  toField = toField . formatTime defaultTimeLocale "%H:%M:%S"

instance FromField TimeOfDay where
  fromField field = do
    value <- fromField field
    parseTimeValue field "%H:%M:%S" value

instance ToField LocalTime where
  toField = toField . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

instance FromField LocalTime where
  fromField field = do
    value <- fromField field
    parseTimeValue field "%Y-%m-%d %H:%M:%S" value

parseTimeValue field format value =
  maybe
    (returnError ConversionFailed field ("Could not parse time: " <> value))
    pure
    (parseTimeM True defaultTimeLocale format value)

-- Database Types
data Coordinate = Coordinate
  { coordinateLatitude :: Scientific,
    coordinateLongitude :: Scientific
  }
  deriving (Generic, Show, Eq)

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
    serviceLocationCoordinate :: Coordinate
  }
  deriving (Generic, Show)

instance FromRow ServiceLocation where
  fromRow =
    ServiceLocation
      <$> field
      <*> field
      <*> field
      <*> (Coordinate <$> field <*> field)

data Location = Location
  { locationLocationID :: Int,
    locationName :: String,
    locationCoordinate :: Coordinate,
    locationCreated :: UTCTime
  }
  deriving (Generic, Show)

instance FromRow Location where
  fromRow =
    Location
      <$> field
      <*> field
      <*> (Coordinate <$> field <*> field)
      <*> field

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
    vesselCoordinate :: Coordinate,
    vesselLastReceived :: UTCTime,
    vesselUpdated :: UTCTime,
    vesselOrganisationID :: Int
  }
  deriving (Generic, Show)

instance FromRow Vessel where
  fromRow =
    Vessel
      <$> field
      <*> field
      <*> field
      <*> field
      <*> (Coordinate <$> field <*> field)
      <*> field
      <*> field
      <*> field

data ServiceVessel = ServiceVessel
  { serviceVesselSeviceID :: Int,
    serviceVesselMmsi :: Int,
    serviceVesselName :: String,
    serviceVesselSpeed :: Maybe Scientific,
    serviceVesselCourse :: Maybe Scientific,
    serviceVesselCoordinate :: Coordinate,
    serviceVesselLastReceived :: UTCTime,
    serviceVesselUpdated :: UTCTime,
    serviceVesselOrganisationID :: Int
  }
  deriving (Generic, Show)

instance FromRow ServiceVessel where
  fromRow =
    ServiceVessel
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> (Coordinate <$> field <*> field)
      <*> field
      <*> field
      <*> field

data LocationDeparture = LocationDeparture
  { locationDepartureFromLocationID :: Int,
    locationDepartureToLocationID :: Int,
    locationDepartureToLocationName :: String,
    locationDepartureToLocationCoordinate :: Coordinate,
    locationDepartureDepartue :: LocalTime,
    locationDepartureArrival :: LocalTime,
    locationDepartureNotes :: Maybe String
  }
  deriving (Generic, Show)

instance FromRow LocationDeparture where
  fromRow =
    LocationDeparture
      <$> field
      <*> field
      <*> field
      <*> (Coordinate <$> field <*> field)
      <*> field
      <*> field
      <*> field

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

data ScrapedTimetableDocument = ScrapedTimetableDocument
  { scrapedTimetableDocumentOrganisationID :: Int,
    scrapedTimetableDocumentServiceIDs :: [Int],
    scrapedTimetableDocumentTitle :: String,
    scrapedTimetableDocumentSourceURL :: String,
    scrapedTimetableDocumentContentHash :: Maybe String,
    scrapedTimetableDocumentContentType :: Maybe String,
    scrapedTimetableDocumentContentLength :: Maybe Int,
    scrapedTimetableDocumentLastSeenAt :: UTCTime
  }
  deriving (Generic, Show)

data TimetableDocument = TimetableDocument
  { timetableDocumentID :: Int,
    timetableDocumentOrganisationID :: Int,
    timetableDocumentOrganisationName :: String,
    timetableDocumentTitle :: String,
    timetableDocumentSourceURL :: String,
    timetableDocumentContentHash :: Maybe String,
    timetableDocumentContentType :: Maybe String,
    timetableDocumentContentLength :: Maybe Int,
    timetableDocumentLastSeenAt :: UTCTime,
    timetableDocumentUpdated :: UTCTime,
    timetableDocumentCreated :: UTCTime
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

jsonOptions :: Typeable a => Proxy a -> Data.Aeson.Options
jsonOptions type' =
  let typeName = show $ typeRep type'
   in defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (length typeName),
          omitNothingFields = True
        }
