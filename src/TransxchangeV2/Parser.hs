{-# LANGUAGE OverloadedStrings #-}

module TransxchangeV2.Parser
  ( findTransxchangeFilesV2,
    fileMayContainFerryServiceV2,
    parseTransxchangeFileV2,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import qualified Data.ByteString.Char8 as BS
import Data.List
  ( find,
    isSuffixOf,
    sort,
  )
import qualified Data.Map.Strict as M
import Data.Maybe
  ( fromMaybe,
  )
import qualified Data.Set as S
import Data.Time.LocalTime
  ( LocalTime,
    TimeOfDay
  )
import Data.Time.Format
  ( defaultTimeLocale,
    formatTime,
    parseTimeM,
  )
import System.Directory (listDirectory)
import Text.XML.Light
  ( Attr (attrKey, attrVal),
    Element (elName),
    QName (QName, qName),
    elChildren,
    elAttribs,
    findAttr,
    onlyElems,
    parseXML,
    strContent,
  )
import TransxchangeV2.Types
import Utility (stringToDay)

findTransxchangeFilesV2 :: FilePath -> IO [FilePath]
findTransxchangeFilesV2 path = do
  files <- findTransxchangeFiles path
  return $ fmap (\filename -> path <> "/" <> filename) files

parseTransxchangeFileV2 :: FilePath -> IO (Either Tx2ParseError Tx2Document)
parseTransxchangeFileV2 = parseFile

fileMayContainFerryServiceV2 :: FilePath -> IO Bool
fileMayContainFerryServiceV2 filePath = do
  fileContents <- try (BS.readFile filePath) :: IO (Either IOException BS.ByteString)
  case fileContents of
    Left _ -> return True
    Right input ->
      return $
        BS.isInfixOf "<mode>ferry</mode>" (BS.map toLowerAscii input)
          || BS.isInfixOf "<mode> ferry </mode>" (BS.map toLowerAscii input)

findTransxchangeFiles :: FilePath -> IO [FilePath]
findTransxchangeFiles path = do
  files <- listDirectory path
  return . sort $ filter (isSuffixOf ".xml") files

parseFile :: FilePath -> IO (Either Tx2ParseError Tx2Document)
parseFile filePath = do
  fileContents <- try (BS.readFile filePath) :: IO (Either IOException BS.ByteString)
  case fileContents of
    Left err ->
      return $
        Left (Tx2ParseError filePath (show err))
    Right input -> do
      case parseXmlDocument filePath (BS.unpack input) of
        Left err -> return $ Left (Tx2ParseError filePath err)
        Right doc -> return $ Right doc

parseXmlDocument :: FilePath -> String -> Either String Tx2Document
parseXmlDocument filePath input = do
  root <- maybeToEither "missing TransXChange root element" $ getTransxchangeRoot input
  let sourceFileName = fromMaybe (takeBaseName filePath) $ attrValueMaybe "FileName" root
  let sourceCreationDateTime = attrValueMaybe "CreationDateTime" root >>= parseXmlDateTime
  let sourceModificationDateTime = attrValueMaybe "ModificationDateTime" root >>= parseXmlDateTime
  let stopPoints = parseStopPoints root
  serviceEntries <- parseServices root
  let services = fmap (\(service, _, _, _) -> service) serviceEntries
  let lines = concatMap (\(_, serviceLines, _, _) -> serviceLines) serviceEntries
  let journeyPatterns = concatMap (\(_, _, patterns, _) -> patterns) serviceEntries
  let journeyPatternSections = concatMap (\(_, _, _, sections) -> sections) serviceEntries
  let journeyPatternTimingLinks = parseJourneyPatternTimingLinks root
  vehicleJourneys <- parseVehicleJourneys root
  return $
    Tx2Document
      { tx2SourcePath = filePath,
        tx2SourceFileName = sourceFileName,
        tx2SourceVersionKey = renderVersionKey sourceFileName sourceCreationDateTime sourceModificationDateTime,
        tx2SourceCreationDateTime = sourceCreationDateTime,
        tx2SourceModificationDateTime = sourceModificationDateTime,
        tx2StopPoints = stopPoints,
        tx2Services = services,
        tx2Lines = lines,
        tx2JourneyPatterns = journeyPatterns,
        tx2JourneyPatternSections = journeyPatternSections,
        tx2JourneyPatternTimingLinks = journeyPatternTimingLinks,
        tx2VehicleJourneys = vehicleJourneys
      }

getTransxchangeRoot :: String -> Maybe Element
getTransxchangeRoot input =
  let xml = onlyElems $ parseXML input
   in find (\element -> qName (elName element) == "TransXChange") xml

parseServices :: Element -> Either String [(Tx2Service, [Tx2Line], [Tx2JourneyPattern], [Tx2JourneyPatternSection])]
parseServices root = do
  servicesNode <- requiredChild "Services" root
  let serviceNodes = childrenNamed "Service" servicesNode
  mapM parseSingleService serviceNodes

parseSingleService :: Element -> Either String (Tx2Service, [Tx2Line], [Tx2JourneyPattern], [Tx2JourneyPatternSection])
parseSingleService serviceNode = do
  serviceCode <- requiredChildText "ServiceCode" serviceNode
  let operatingPeriod = childNamed "OperatingPeriod" serviceNode
  let startDate = operatingPeriod >>= childText "StartDate" >>= stringToDay
  let endDate = operatingPeriod >>= childText "EndDate" >>= stringToDay
  let operatorRef = fromMaybe "" $ childText "RegisteredOperatorRef" serviceNode
  let mode = fromMaybe "" $ childText "Mode" serviceNode
  let description = fromMaybe "" $ childText "Description" serviceNode
  let standardServiceNode = childNamed "StandardService" serviceNode
  let origin = maybe "" (fromMaybe "" . childText "Origin") standardServiceNode
  let destination = maybe "" (fromMaybe "" . childText "Destination") standardServiceNode
  let lines = parseLines serviceCode serviceNode
  let (journeyPatterns, journeyPatternSections) = maybe ([], []) (parseJourneyPatterns serviceCode) standardServiceNode
  let service =
        Tx2Service
          { tx2ServiceCode = serviceCode,
            tx2OperatorRef = operatorRef,
            tx2Mode = mode,
            tx2Description = description,
            tx2Origin = origin,
            tx2Destination = destination,
            tx2StartDate = startDate,
            tx2EndDate = endDate
          }
  return (service, lines, journeyPatterns, journeyPatternSections)

parseLines :: String -> Element -> [Tx2Line]
parseLines serviceCode serviceNode =
  case childNamed "Lines" serviceNode of
    Nothing -> []
    Just linesNode ->
      fmap
        ( \lineNode ->
            Tx2Line
              { tx2LineId = attrValue "id" lineNode,
                tx2LineServiceCode = serviceCode,
                tx2LineName = fromMaybe "" $ childText "LineName" lineNode
              }
        )
        (childrenNamed "Line" linesNode)

parseJourneyPatterns :: String -> Element -> ([Tx2JourneyPattern], [Tx2JourneyPatternSection])
parseJourneyPatterns serviceCode standardServiceNode =
  unzipJourneyPatterns (childrenNamed "JourneyPattern" standardServiceNode)
  where
    unzipJourneyPatterns patternNodes =
      let parsed = fmap parseJourneyPattern patternNodes
       in (fmap fst parsed, concatMap snd parsed)

    parseJourneyPattern patternNode =
      let patternId = attrValue "id" patternNode
          sectionRefs = fmap strContent (childrenNamed "JourneyPatternSectionRefs" patternNode)
          pattern =
            Tx2JourneyPattern
              { tx2JourneyPatternId = patternId,
                tx2JourneyPatternServiceCode = serviceCode,
                tx2JourneyPatternDirection = fromMaybe "" $ childText "Direction" patternNode
              }
          patternSections =
            zipWith
              (\sectionOrder sectionRef -> Tx2JourneyPatternSection patternId sectionRef sectionOrder)
              [1 ..]
              sectionRefs
       in (pattern, patternSections)

parseStopPoints :: Element -> [Tx2StopPoint]
parseStopPoints root =
  case childNamed "StopPoints" root of
    Nothing -> []
    Just stopPointsNode ->
      fmap parseStopPoint (childrenNamed "AnnotatedStopPointRef" stopPointsNode)
  where
    parseStopPoint :: Element -> Tx2StopPoint
    parseStopPoint stopPointNode =
      Tx2StopPoint
        { tx2StopPointRef = fromMaybe "" $ childText "StopPointRef" stopPointNode,
          tx2StopPointCommonName = fromMaybe "" $ childText "CommonName" stopPointNode
        }

parseJourneyPatternTimingLinks :: Element -> [Tx2JourneyPatternTimingLink]
parseJourneyPatternTimingLinks root =
  case childNamed "JourneyPatternSections" root of
    Nothing -> []
    Just sectionsNode ->
      concatMap parseSection (childrenNamed "JourneyPatternSection" sectionsNode)
  where
    parseSection :: Element -> [Tx2JourneyPatternTimingLink]
    parseSection sectionNode =
      let sectionRef = attrValue "id" sectionNode
          timingLinks = childrenNamed "JourneyPatternTimingLink" sectionNode
       in zipWith (parseTimingLink sectionRef) [1 ..] timingLinks

    parseTimingLink :: String -> Int -> Element -> Tx2JourneyPatternTimingLink
    parseTimingLink sectionRef sortOrder timingLinkNode =
      Tx2JourneyPatternTimingLink
        { tx2JourneyPatternTimingLinkId = attrValue "id" timingLinkNode,
          tx2JourneyPatternTimingLinkSectionRef = sectionRef,
          tx2JourneyPatternTimingLinkSortOrder = sortOrder,
          tx2JourneyPatternTimingLinkFromStopPointRef =
            fromMaybe "" $
              childNamed "From" timingLinkNode >>= childText "StopPointRef",
          tx2JourneyPatternTimingLinkToStopPointRef =
            fromMaybe "" $
              childNamed "To" timingLinkNode >>= childText "StopPointRef",
          tx2JourneyPatternTimingLinkRouteLinkRef =
            fromMaybe "" $
              childText "RouteLinkRef" timingLinkNode,
          tx2JourneyPatternTimingLinkDirection =
            fromMaybe "" $
              childText "Direction" timingLinkNode,
          tx2JourneyPatternTimingLinkRunTime =
            fromMaybe "" $
              childText "RunTime" timingLinkNode,
          tx2JourneyPatternTimingLinkFromWaitTime =
            fromMaybe "" $
              childNamed "From" timingLinkNode >>= childText "WaitTime"
        }

parseVehicleJourneys :: Element -> Either String [Tx2VehicleJourney]
parseVehicleJourneys root =
  case childNamed "VehicleJourneys" root of
    Nothing -> Right []
    Just journeysNode -> do
      let journeyNodes = childrenNamed "VehicleJourney" journeysNode
      rawJourneys <- mapM parseVehicleJourneyRaw journeyNodes
      resolveVehicleJourneys rawJourneys

data RawTx2VehicleJourney = RawTx2VehicleJourney
  { rawVehicleJourneyCode :: String,
    rawVehicleJourneyRef :: Maybe String,
    rawServiceRef :: Maybe String,
    rawLineRef :: Maybe String,
    rawJourneyPatternRef :: Maybe String,
    rawOperatorRef :: Maybe String,
    rawDepartureTime :: Maybe TimeOfDay,
    rawDayRules :: Maybe [Tx2DayRule],
    rawDaysOfOperation :: Maybe [Tx2DateRange],
    rawDaysOfNonOperation :: Maybe [Tx2DateRange],
    rawNote :: Maybe String,
    rawNoteCode :: Maybe String
  }
  deriving (Show, Eq)

parseVehicleJourneyRaw :: Element -> Either String RawTx2VehicleJourney
parseVehicleJourneyRaw journeyNode = do
  vehicleJourneyCode <- requiredChildText "VehicleJourneyCode" journeyNode
  departureTime <- parseOptionalDepartureTime journeyNode
  let serviceRef = childText "ServiceRef" journeyNode
  let lineRef = childText "LineRef" journeyNode
  let journeyPatternRef = childText "JourneyPatternRef" journeyNode
  let operatorRef = childText "OperatorRef" journeyNode
  let journeyRef = childText "VehicleJourneyRef" journeyNode
  let note = childNamed "Note" journeyNode >>= childText "NoteText"
  let noteCode = childNamed "Note" journeyNode >>= childText "NoteCode"
  let dayRules = parseDayRules journeyNode
  let daysOfOperation = parseSpecialDays "SpecialDaysOperation" journeyNode
  let daysOfNonOperation = parseSpecialDays "SpecialDaysNonOperation" journeyNode
  return $
    RawTx2VehicleJourney
      { rawVehicleJourneyCode = vehicleJourneyCode,
        rawVehicleJourneyRef = journeyRef,
        rawServiceRef = serviceRef,
        rawLineRef = lineRef,
        rawJourneyPatternRef = journeyPatternRef,
        rawOperatorRef = operatorRef,
        rawDepartureTime = departureTime,
        rawDayRules = dayRules,
        rawDaysOfOperation = daysOfOperation,
        rawDaysOfNonOperation = daysOfNonOperation,
        rawNote = note,
        rawNoteCode = noteCode
      }

parseOptionalDepartureTime :: Element -> Either String (Maybe TimeOfDay)
parseOptionalDepartureTime journeyNode =
  case childText "DepartureTime" journeyNode of
    Nothing -> Right Nothing
    Just departureString ->
      case parseTimeOfDay departureString of
        Nothing -> Left ("invalid departure time: " <> departureString)
        Just value -> Right (Just value)

resolveVehicleJourneys :: [RawTx2VehicleJourney] -> Either String [Tx2VehicleJourney]
resolveVehicleJourneys rawJourneys = mapM (resolveByCode S.empty . rawVehicleJourneyCode) rawJourneys
  where
    journeyMap = M.fromList [(rawVehicleJourneyCode journey, journey) | journey <- rawJourneys]

    resolveByCode :: S.Set String -> String -> Either String Tx2VehicleJourney
    resolveByCode visited code = do
      rawJourney <- maybeToEither ("vehicle journey not found: " <> code) (M.lookup code journeyMap)
      resolvedRaw <- resolveRaw visited rawJourney
      materialize resolvedRaw

    resolveRaw :: S.Set String -> RawTx2VehicleJourney -> Either String RawTx2VehicleJourney
    resolveRaw visited rawJourney =
      case rawVehicleJourneyRef rawJourney of
        Nothing -> Right rawJourney
        Just baseCode -> do
          if S.member baseCode visited
            then Left ("vehicle journey reference cycle: " <> baseCode)
            else do
              base <- maybeToEither ("vehicle journey reference not found: " <> baseCode) (M.lookup baseCode journeyMap)
              resolvedBase <- resolveRaw (S.insert baseCode visited) base
              Right (mergeRawJourney resolvedBase rawJourney)

    mergeRawJourney :: RawTx2VehicleJourney -> RawTx2VehicleJourney -> RawTx2VehicleJourney
    mergeRawJourney base child =
      RawTx2VehicleJourney
        { rawVehicleJourneyCode = rawVehicleJourneyCode child,
          rawVehicleJourneyRef = rawVehicleJourneyRef child,
          rawServiceRef = rawServiceRef child <|> rawServiceRef base,
          rawLineRef = rawLineRef child <|> rawLineRef base,
          rawJourneyPatternRef = rawJourneyPatternRef child <|> rawJourneyPatternRef base,
          rawOperatorRef = rawOperatorRef child <|> rawOperatorRef base,
          rawDepartureTime = rawDepartureTime child <|> rawDepartureTime base,
          rawDayRules = rawDayRules child <|> rawDayRules base,
          rawDaysOfOperation = rawDaysOfOperation child <|> rawDaysOfOperation base,
          rawDaysOfNonOperation = rawDaysOfNonOperation child <|> rawDaysOfNonOperation base,
          rawNote = rawNote child <|> rawNote base,
          rawNoteCode = rawNoteCode child <|> rawNoteCode base
        }

    materialize :: RawTx2VehicleJourney -> Either String Tx2VehicleJourney
    materialize rawJourney = do
      serviceRef <- maybeToEither "missing ServiceRef on resolved vehicle journey" (rawServiceRef rawJourney)
      lineRef <- maybeToEither "missing LineRef on resolved vehicle journey" (rawLineRef rawJourney)
      journeyPatternRef <- maybeToEither "missing JourneyPatternRef on resolved vehicle journey" (rawJourneyPatternRef rawJourney)
      departureTime <- maybeToEither "missing DepartureTime on resolved vehicle journey" (rawDepartureTime rawJourney)
      let operatorRef = fromMaybe "" (rawOperatorRef rawJourney)
      let note = fromMaybe "" (rawNote rawJourney)
      let noteCode = fromMaybe "" (rawNoteCode rawJourney)
      let dayRules = fromMaybe [] (rawDayRules rawJourney)
      let daysOfOperation = fromMaybe [] (rawDaysOfOperation rawJourney)
      let daysOfNonOperation = fromMaybe [] (rawDaysOfNonOperation rawJourney)
      return $
        Tx2VehicleJourney
          { tx2VehicleJourneyCode = rawVehicleJourneyCode rawJourney,
            tx2VehicleJourneyServiceCode = serviceRef,
            tx2VehicleJourneyLineId = lineRef,
            tx2VehicleJourneyPatternId = journeyPatternRef,
            tx2VehicleJourneyOperatorRef = operatorRef,
            tx2VehicleJourneyDepartureTime = departureTime,
            tx2VehicleJourneyDayRules = dayRules,
            tx2VehicleJourneyDaysOfOperation = daysOfOperation,
            tx2VehicleJourneyDaysOfNonOperation = daysOfNonOperation,
            tx2VehicleJourneyNote = note,
            tx2VehicleJourneyNoteCode = noteCode
          }

parseDayRules :: Element -> Maybe [Tx2DayRule]
parseDayRules journeyNode =
  case childNamed "OperatingProfile" journeyNode >>= childNamed "RegularDayType" of
    Nothing -> Nothing
    Just regularDayType ->
      case childNamed "HolidaysOnly" regularDayType of
        Just _ -> Just [Tx2HolidaysOnly]
        Nothing ->
          case childNamed "DaysOfWeek" regularDayType of
            Nothing -> Just []
            Just daysOfWeekNode -> Just (concatMap dayElementToDays (elChildren daysOfWeekNode))

parseSpecialDays :: String -> Element -> Maybe [Tx2DateRange]
parseSpecialDays nodeName journeyNode =
  case childNamed nodeName journeyNode of
    Nothing -> Nothing
    Just specialDaysNode ->
      let ranges =
            [ Tx2DateRange startDate endDate
            | dateRangeNode <- childrenNamed "DateRange" specialDaysNode,
              Just startDate <- [childText "StartDate" dateRangeNode >>= stringToDay],
              Just endDate <- [childText "EndDate" dateRangeNode >>= stringToDay]
            ]
       in Just ranges

dayElementToDays :: Element -> [Tx2DayRule]
dayElementToDays element =
  case qName (elName element) of
    "Monday" -> [Tx2Monday]
    "Tuesday" -> [Tx2Tuesday]
    "Wednesday" -> [Tx2Wednesday]
    "Thursday" -> [Tx2Thursday]
    "Friday" -> [Tx2Friday]
    "Saturday" -> [Tx2Saturday]
    "Sunday" -> [Tx2Sunday]
    "MondayToFriday" -> [Tx2MondayToFriday]
    "MondayToSaturday" -> [Tx2MondayToSaturday]
    "MondayToSunday" -> [Tx2MondayToSunday]
    "Weekend" -> [Tx2Weekend]
    "NotMonday" -> [Tx2NotMonday]
    "NotTuesday" -> [Tx2NotTuesday]
    "NotWednesday" -> [Tx2NotWednesday]
    "NotThursday" -> [Tx2NotThursday]
    "NotFriday" -> [Tx2NotFriday]
    "NotSaturday" -> [Tx2NotSaturday]
    "NotSunday" -> [Tx2NotSunday]
    _ -> []

parseTimeOfDay :: String -> Maybe TimeOfDay
parseTimeOfDay value =
  parseTimeM True defaultTimeLocale "%H:%M:%S" value
    <|> parseTimeM True defaultTimeLocale "%H:%M" value

requiredChild :: String -> Element -> Either String Element
requiredChild name element =
  maybeToEither ("missing child: " <> name) (childNamed name element)

requiredChildText :: String -> Element -> Either String String
requiredChildText name element =
  maybeToEither ("missing child text: " <> name) (childText name element)

childNamed :: String -> Element -> Maybe Element
childNamed name element = find (\child -> qName (elName child) == name) (elChildren element)

childrenNamed :: String -> Element -> [Element]
childrenNamed name element = filter (\child -> qName (elName child) == name) (elChildren element)

childText :: String -> Element -> Maybe String
childText name element = strContent <$> childNamed name element

attrValue :: String -> Element -> String
attrValue name element = fromMaybe "" $ findAttr (QName name Nothing Nothing) element

attrValueMaybe :: String -> Element -> Maybe String
attrValueMaybe name element =
  attrVal <$> find (\attribute -> qName (attrKey attribute) == name) (elAttribs element)

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither message = maybe (Left message) Right

toLowerAscii :: Char -> Char
toLowerAscii value
  | 'A' <= value && value <= 'Z' = toEnum (fromEnum value + 32)
  | otherwise = value

parseXmlDateTime :: String -> Maybe LocalTime
parseXmlDateTime =
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q"

renderVersionKey :: String -> Maybe LocalTime -> Maybe LocalTime -> String
renderVersionKey sourceFileName sourceCreationDateTime sourceModificationDateTime =
  sourceFileName
    <> "|"
    <> renderMaybeDateTime sourceCreationDateTime
    <> "|"
    <> renderMaybeDateTime sourceModificationDateTime

renderMaybeDateTime :: Maybe LocalTime -> String
renderMaybeDateTime =
  maybe "" (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q")

takeBaseName :: FilePath -> FilePath
takeBaseName = reverse . takeWhile (/= '/') . reverse
