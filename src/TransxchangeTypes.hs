module TransxchangeTypes where

import Data.Binary.Builder (putStringUtf8)
import Data.Char (toLower)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple.ToField
  ( Action (Plain),
    ToField (..),
  )

data TransXChangeData = TransXChangeData
  { stopPoints :: [AnnotatedStopPointRef],
    servicedOrganisations :: [ServicedOrganisation],
    routeSections :: [RouteSection],
    routes :: [Route],
    journeyPatternSections :: [JourneyPatternSection],
    operators :: [Operator],
    services :: [Service],
    vehicleJourneys :: [VehicleJourney]
  }
  deriving (Show)

data ServicedOrganisation = ServicedOrganisation
  { organisationCode :: String,
    organisationName :: String,
    organisationWorkingDays :: [DateRange]
  }
  deriving (Show)

data AnnotatedStopPointRef = AnnotatedStopPointRef
  { stopPointRef :: String,
    commonName :: String
  }
  deriving (Show)

data RouteSection = RouteSection
  { routeSectionId :: String,
    routeLinks :: [RouteLink]
  }
  deriving (Show)

data RouteLink = RouteLink
  { routeLinkId :: String,
    fromStopPointRef :: String,
    toStopPointRef :: String,
    routeDirection :: String
  }
  deriving (Show)

data Route = Route
  { routeId :: String,
    routeDescription :: String,
    routeSectionRef :: String
  }
  deriving (Show)

data JourneyPatternSection = JourneyPatternSection
  { journeyPatterSectionId :: String,
    journeyPatternTimingLinks :: [JourneyPatternTimingLink]
  }
  deriving (Show)

data JourneyPatternTimingLink = JourneyPatternTimingLink
  { journeyPatternTimingLinkId :: String,
    journeyPatternFromWaitTime :: String,
    journeyPatternFromStopPointRef :: String,
    journeyPatternFromTimingStatus :: String,
    journeyPatternToStopPointsRef :: String,
    journeyPatternToTimingStatus :: String,
    routeLinkRef :: String,
    journeyDirection :: String,
    runTime :: String
  }
  deriving (Show)

data Operator = Operator
  { operatorId :: String,
    nationalOperatorCode :: String,
    operatorCode :: String,
    operatorShortName :: String
  }
  deriving (Show)

data Service = Service
  { serviceCode :: String,
    lines :: [Line],
    operatingPeriod :: DateRange,
    registeredOperatorRef :: String,
    mode :: String,
    description :: String,
    standardService :: StandardService
  }
  deriving (Show)

data Line = Line
  { lineId :: String,
    lineName :: String
  }
  deriving (Show)

data StandardService = StandardService
  { origin :: String,
    destination :: String,
    journeyPatterns :: [JourneyPattern]
  }
  deriving (Show)

data JourneyPattern = JourneyPattern
  { journeyPatternId :: String,
    journeyPatternDirection :: String,
    journeyPatternSectionRef :: String
  }
  deriving (Show)

data VehicleJourney = VehicleJourney
  { operatorRef :: String,
    vehicleJourneyCode :: String,
    serviceRef :: String,
    lineRef :: String,
    journeyPatternRef :: String,
    departureTime :: String,
    daysOfWeek :: [WeekDay],
    specialDaysOfOperation :: [DateRange],
    specialDaysOfNonOperation :: [DateRange],
    note :: String,
    noteCode :: String,
    daysOfNonOperationServicedOrganisationRef :: Maybe String
  }
  deriving (Show)

data DateRange = DateRange
  { startDate :: Maybe Day,
    endDate :: Maybe Day
  }
  deriving (Show)

data WeekDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  | HolidaysOnly
  deriving (Show, Eq)

instance ToField WeekDay where
  toField a =
    Plain $ putStringUtf8 $ "'" <> (toLower <$> show a) <> "'::day_of_week"