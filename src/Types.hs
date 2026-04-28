{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import App.Env (Application, logger)
import App.Logger (MonadLogger (askLogger))
import Control.Monad.Reader (asks)
import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier, omitNothingFields),
    ToJSON (toJSON),
    camelTo2,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
    withScientific,
  )
import qualified Data.ByteString.Lazy as B
import Data.Char (toLower, toUpper)
import Data.Maybe (fromMaybe)
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
  ( FromRow,
    ToRow,
  )
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import Web.Scotty.Trans (ActionT, ScottyT)

-- Web server
type Scotty = ScottyT Application ()

type Action = ActionT Application

instance MonadLogger Types.Action where
  askLogger = asks logger

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
    calMacAPIResponseRouteLocation :: CalMacAPIResponseRouteLocation,
    calMacAPIResponseRouteRouteStatuses :: [CalMacAPIResponseRouteStatus]
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

data CalMacAPIResponseRouteStatus = CalMacAPIResponseRouteStatus
  { calMacAPIResponseRouteStatusTitle :: String,
    calMacAPIResponseRouteStatusStatus :: String,
    calMacAPIResponseRouteStatusDetail :: String
  }
  deriving (Generic, Show)

instance FromJSON CalMacAPIResponseRouteStatus where
  parseJSON = genericParseJSON $ calMacAPIResponseJsonOptions (Proxy :: Proxy CalMacAPIResponseRouteStatus)

calMacAPIResponseJsonOptions :: Typeable a => Proxy a -> Data.Aeson.Options
calMacAPIResponseJsonOptions type' =
  let typeName = show $ typeRep type'
   in defaultOptions
        { fieldLabelModifier = toLowerFirstLetter . drop (length typeName),
          omitNothingFields = True
        }

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
