module TransxchangeV2.Transform
  ( filterFerryDocuments,
    isFerryMode,
  )
where

import Data.Char (toLower)
import Data.List (any)
import TransxchangeV2.Types

filterFerryDocuments :: [Tx2Document] -> [Tx2Document]
filterFerryDocuments = foldr keepFerryOnly []
  where
    keepFerryOnly doc acc =
      let ferryServices = filter (isFerryMode . tx2Mode) (tx2Services doc)
          ferryServiceCodes = fmap tx2ServiceCode ferryServices
          ferryLines = filter (\line -> tx2LineServiceCode line `elem` ferryServiceCodes) (tx2Lines doc)
          ferryPatterns = filter (\pattern -> tx2JourneyPatternServiceCode pattern `elem` ferryServiceCodes) (tx2JourneyPatterns doc)
          ferryJourneys =
            filter
              (\journey -> tx2VehicleJourneyServiceCode journey `elem` ferryServiceCodes)
              (tx2VehicleJourneys doc)
       in if null ferryServices
            then acc
            else
              Tx2Document
                { tx2SourcePath = tx2SourcePath doc,
                  tx2SourceFileName = tx2SourceFileName doc,
                  tx2SourceVersionKey = tx2SourceVersionKey doc,
                  tx2SourceCreationDateTime = tx2SourceCreationDateTime doc,
                  tx2SourceModificationDateTime = tx2SourceModificationDateTime doc,
                  tx2StopPoints = tx2StopPoints doc,
                  tx2Services = ferryServices,
                  tx2Lines = ferryLines,
                  tx2JourneyPatterns = ferryPatterns,
                  tx2JourneyPatternTimingLinks = tx2JourneyPatternTimingLinks doc,
                  tx2VehicleJourneys = ferryJourneys
                } :
              acc

isFerryMode :: String -> Bool
isFerryMode value = map toLower value == "ferry"
