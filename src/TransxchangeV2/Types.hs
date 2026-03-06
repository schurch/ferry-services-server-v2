module TransxchangeV2.Types where

import Data.Time.Calendar (Day)
import Data.Time.LocalTime
  ( LocalTime,
    TimeOfDay,
  )

data Tx2Document = Tx2Document
  { tx2SourcePath :: FilePath,
    tx2SourceFileName :: String,
    tx2SourceVersionKey :: String,
    tx2SourceCreationDateTime :: Maybe LocalTime,
    tx2SourceModificationDateTime :: Maybe LocalTime,
    tx2StopPoints :: [Tx2StopPoint],
    tx2Services :: [Tx2Service],
    tx2Lines :: [Tx2Line],
    tx2JourneyPatterns :: [Tx2JourneyPattern],
    tx2JourneyPatternTimingLinks :: [Tx2JourneyPatternTimingLink],
    tx2VehicleJourneys :: [Tx2VehicleJourney]
  }
  deriving (Show, Eq)

data Tx2StopPoint = Tx2StopPoint
  { tx2StopPointRef :: String,
    tx2StopPointCommonName :: String
  }
  deriving (Show, Eq)

data Tx2Service = Tx2Service
  { tx2ServiceCode :: String,
    tx2OperatorRef :: String,
    tx2Mode :: String,
    tx2Description :: String,
    tx2Origin :: String,
    tx2Destination :: String,
    tx2StartDate :: Maybe Day,
    tx2EndDate :: Maybe Day
  }
  deriving (Show, Eq)

data Tx2Line = Tx2Line
  { tx2LineId :: String,
    tx2LineServiceCode :: String,
    tx2LineName :: String
  }
  deriving (Show, Eq)

data Tx2JourneyPattern = Tx2JourneyPattern
  { tx2JourneyPatternId :: String,
    tx2JourneyPatternServiceCode :: String,
    tx2JourneyPatternSectionRef :: String,
    tx2JourneyPatternDirection :: String
  }
  deriving (Show, Eq)

data Tx2JourneyPatternTimingLink = Tx2JourneyPatternTimingLink
  { tx2JourneyPatternTimingLinkId :: String,
    tx2JourneyPatternTimingLinkSectionRef :: String,
    tx2JourneyPatternTimingLinkSortOrder :: Int,
    tx2JourneyPatternTimingLinkFromStopPointRef :: String,
    tx2JourneyPatternTimingLinkToStopPointRef :: String,
    tx2JourneyPatternTimingLinkRouteLinkRef :: String,
    tx2JourneyPatternTimingLinkDirection :: String,
    tx2JourneyPatternTimingLinkRunTime :: String,
    tx2JourneyPatternTimingLinkFromWaitTime :: String
  }
  deriving (Show, Eq)

data Tx2DayRule
  = Tx2Monday
  | Tx2Tuesday
  | Tx2Wednesday
  | Tx2Thursday
  | Tx2Friday
  | Tx2Saturday
  | Tx2Sunday
  | Tx2MondayToFriday
  | Tx2MondayToSaturday
  | Tx2MondayToSunday
  | Tx2Weekend
  | Tx2NotMonday
  | Tx2NotTuesday
  | Tx2NotWednesday
  | Tx2NotThursday
  | Tx2NotFriday
  | Tx2NotSaturday
  | Tx2NotSunday
  | Tx2HolidaysOnly
  deriving (Show, Eq, Ord)

data Tx2VehicleJourney = Tx2VehicleJourney
  { tx2VehicleJourneyCode :: String,
    tx2VehicleJourneyServiceCode :: String,
    tx2VehicleJourneyLineId :: String,
    tx2VehicleJourneyPatternId :: String,
    tx2VehicleJourneyOperatorRef :: String,
    tx2VehicleJourneyDepartureTime :: TimeOfDay,
    tx2VehicleJourneyDayRules :: [Tx2DayRule],
    tx2VehicleJourneyNote :: String,
    tx2VehicleJourneyNoteCode :: String
  }
  deriving (Show, Eq)

data Tx2ParseError = Tx2ParseError
  { tx2ParseErrorFile :: FilePath,
    tx2ParseErrorMessage :: String
  }
  deriving (Show, Eq)

data Tx2IngestSummary = Tx2IngestSummary
  { tx2FilesParsed :: Int,
    tx2FilesSkipped :: Int,
    tx2FilesFailed :: Int,
    tx2FerryDocuments :: Int,
    tx2ServicesWritten :: Int,
    tx2JourneyWritten :: Int
  }
  deriving (Show, Eq)
