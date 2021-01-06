{-# LANGUAGE OverloadedStrings #-}

module TransxchangeParser
  ( parseTransxchangeXML
  )
where

import           Data.Maybe                     ( fromMaybe )
import           Data.String                    ( IsString(..) )
import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , parseTimeOrError
                                                )
import           Text.XML.Light                 ( QName(QName, qName)
                                                , parseXML
                                                , elChildren
                                                , findAttr
                                                , findChild
                                                , findChildren
                                                , onlyElems
                                                , strContent
                                                , Element(elName)
                                                )
import           TransxchangeTypes
-- import           Debug.Trace

parseTransxchangeXML :: String -> TransXChangeData
parseTransxchangeXML input = TransXChangeData
  { stopPoints             = fromMaybe [] $ getStopPoints transXChangeElement
  , routeSections          = fromMaybe [] $ getRouteSections transXChangeElement
  , routes                 = fromMaybe [] $ getRoutes transXChangeElement
  , journeyPatternSections = fromMaybe []
                               $ getJourneyPatternSections transXChangeElement
  , operators              = fromMaybe [] $ getOperators transXChangeElement
  , services               = fromMaybe [] $ getServices transXChangeElement
  , vehicleJourneys = fromMaybe [] $ getVehicleJourneys transXChangeElement
  }
 where
  xml                 = onlyElems $ parseXML input
  transXChangeElement = xml !! 1

-- Helpers
instance IsString QName where
  fromString name = QName name (Just "http://www.transxchange.org.uk/") Nothing

attr :: String -> QName
attr name = QName name Nothing Nothing

attrValue :: String -> Element -> String
attrValue name element = fromMaybe "" $ findAttr (attr name) element

parseXMLDate :: String -> UTCTime
parseXMLDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

-- Stop Points
getStopPoints :: Element -> Maybe [AnnotatedStopPointRef]
getStopPoints element = do
  stopPointElement <- findChild "StopPoints" element
  let stopPointElements = findChildren "AnnotatedStopPointRef" stopPointElement
  return $ elementToStopPoint <$> stopPointElements
 where
  elementToStopPoint :: Element -> AnnotatedStopPointRef
  elementToStopPoint stopPointElement = AnnotatedStopPointRef
    { stopPointRef = maybe ""
                           strContent
                           (findChild "StopPointRef" stopPointElement)
    , commonName = maybe "" strContent (findChild "CommonName" stopPointElement)
    }

-- Route Sections
getRouteSections :: Element -> Maybe [RouteSection]
getRouteSections element = do
  routeSectionsElement <- findChild "RouteSections" element
  let routeSectionElements = findChildren "RouteSection" routeSectionsElement
  return $ elementToRouteSection <$> routeSectionElements
 where
  elementToRouteSection :: Element -> RouteSection
  elementToRouteSection element = RouteSection
    { routeSectionId = attrValue "id" element
    , routeLinks     = elementToRouteLink <$> findChildren "RouteLink" element
    }

  elementToRouteLink :: Element -> RouteLink
  elementToRouteLink element = RouteLink
    { routeLinkId      = attrValue "id" element
    , fromStopPointRef = maybe
                           ""
                           strContent
                           (   findChild "From" element
                           >>= findChild "StopPointRef"
                           )
    , toStopPointRef   = maybe
                           ""
                           strContent
                           (findChild "To" element >>= findChild "StopPointRef")
    , routeDirection   = maybe "" strContent (findChild "Direction" element)
    }

-- Routes
getRoutes :: Element -> Maybe [Route]
getRoutes element = do
  routesElement <- findChild "Routes" element
  let routeElements = findChildren "Route" routesElement
  return $ elementToRoute <$> routeElements
 where
  elementToRoute :: Element -> Route
  elementToRoute element = Route
    { routeId          = attrValue "id" element
    , routeDescription = maybe "" strContent (findChild "Description" element)
    , routeSectionRef  = maybe ""
                               strContent
                               (findChild "RouteSectionRef" element)
    }

-- JourneyPatternSection
getJourneyPatternSections :: Element -> Maybe [JourneyPatternSection]
getJourneyPatternSections element = do
  journeyPatternSectionsElement <- findChild "JourneyPatternSections" element
  let journeyPatternSectionsElements =
        findChildren "JourneyPatternSection" journeyPatternSectionsElement
  return $ elementToJourneyPatternSection <$> journeyPatternSectionsElements
 where
  elementToJourneyPatternSection :: Element -> JourneyPatternSection
  elementToJourneyPatternSection element = JourneyPatternSection
    { journeyPatterSectionId    = attrValue "id" element
    , journeyPatternTimingLinks = elementToJourneyPatternTimingLink
                                    <$> findChildren
                                          "JourneyPatternTimingLink"
                                          element
    }

  elementToJourneyPatternTimingLink :: Element -> JourneyPatternTimingLink
  elementToJourneyPatternTimingLink element = JourneyPatternTimingLink
    { journeyPatternTimingLinkId = attrValue "id" element
    , journeyPatternFromWaitTime = maybe
                                     ""
                                     strContent
                                     (   findChild "From" element
                                     >>= findChild "WaitTime"
                                     )
    , journeyPatternFromStopPointRef = maybe
                                         ""
                                         strContent
                                         (   findChild "From" element
                                         >>= findChild "StopPointRef"
                                         )
    , journeyPatternFromTimingStatus = maybe
                                         ""
                                         strContent
                                         (   findChild "From" element
                                         >>= findChild "TimingStatus"
                                         )
    , journeyPatternToStopPointsRef = maybe
                                        ""
                                        strContent
                                        (   findChild "To" element
                                        >>= findChild "StopPointRef"
                                        )
    , journeyPatternToTimingStatus = maybe
                                       ""
                                       strContent
                                       (   findChild "To" element
                                       >>= findChild "TimingStatus"
                                       )
    , routeLinkRef = maybe "" strContent (findChild "RouteLinkRef" element)
    , journeyDirection = maybe "" strContent (findChild "Direction" element)
    , runTime = maybe "" strContent (findChild "RunTime" element)
    }

-- Operators
getOperators :: Element -> Maybe [Operator]
getOperators element = do
  operatorsElement <- findChild "Operators" element
  let operatorsElements = findChildren "Operator" operatorsElement
  return $ elementToOperator <$> operatorsElements
 where
  elementToOperator :: Element -> Operator
  elementToOperator element = Operator
    { operatorId           = attrValue "id" element
    , nationalOperatorCode = maybe ""
                                   strContent
                                   (findChild "NationalOperatorCode" element)
    , operatorCode = maybe "" strContent (findChild "OperatorCode" element)
    , operatorShortName    = maybe ""
                                   strContent
                                   (findChild "OperatorShortName" element)
    }

-- Services
getServices :: Element -> Maybe [Service]
getServices element = do
  servicesElement <- findChild "Services" element
  let serviceElements = findChildren "Service" servicesElement
  return $ elementToService <$> serviceElements
 where
  elementToService :: Element -> Service
  elementToService element = Service
    { serviceCode = maybe "" strContent (findChild "ServiceCode" element)
    , TransxchangeTypes.lines = fromMaybe [] $ getLines element
    , operatingPeriod         = maybe (DateRange Nothing Nothing)
                                      elementToDateRange
                                      (findChild "OperatingPeriod" element)
    , registeredOperatorRef   = maybe
                                  ""
                                  strContent
                                  (findChild "RegisteredOperatorRef" element)
    , mode                    = maybe "" strContent (findChild "Mode" element)
    , description = maybe "" strContent (findChild "Description" element)
    , standardService         = maybe (StandardService "" "" [])
                                      elementToStandardService
                                      (findChild "StandardService" element)
    }

-- Lines
getLines :: Element -> Maybe [TransxchangeTypes.Line]
getLines element = do
  linesElement <- findChild "Lines" element
  let lineElements = findChildren "Line" linesElement
  return $ elementToLine <$> lineElements
 where
  elementToLine :: Element -> TransxchangeTypes.Line
  elementToLine element = TransxchangeTypes.Line
    { lineId   = attrValue "id" element
    , lineName = maybe "" strContent (findChild "LineName" element)
    }

-- Standard Service
elementToStandardService :: Element -> StandardService
elementToStandardService element = StandardService
  { origin          = maybe "" strContent (findChild "Origin" element)
  , destination     = maybe "" strContent (findChild "Destination" element)
  , journeyPatterns = elementToJourneyPattern
                        <$> findChildren "JourneyPattern" element
  }
 where
  elementToJourneyPattern :: Element -> JourneyPattern
  elementToJourneyPattern element = JourneyPattern
    { journeyPatternId         = attrValue "id" element
    , journeyPatternDirection  = maybe ""
                                       strContent
                                       (findChild "Direction" element)
    , journeyPatternSectionRef = maybe
                                   ""
                                   strContent
                                   (findChild "JourneyPatternSectionRefs"
                                              element
                                   )
    }

-- Vehicle journeys
getVehicleJourneys :: Element -> Maybe [VehicleJourney]
getVehicleJourneys element = do
  vehicleJourneysElement <- findChild "VehicleJourneys" element
  let vehicleJourneyElements =
        findChildren "VehicleJourney" vehicleJourneysElement
  return $ elementToVehicleJourney <$> vehicleJourneyElements
 where
  elementToVehicleJourney :: Element -> VehicleJourney
  elementToVehicleJourney element = VehicleJourney
    { operatorRef = maybe "" strContent (findChild "OperatorRef" element)
    , vehicleJourneyCode = maybe ""
                                 strContent
                                 (findChild "VehicleJourneyCode" element)
    , serviceRef = maybe "" strContent (findChild "ServiceRef" element)
    , lineRef = maybe "" strContent (findChild "LineRef" element)
    , journeyPatternRef = maybe ""
                                strContent
                                (findChild "JourneyPatternRef" element)
    , departureTime = maybe "" strContent (findChild "DepartureTime" element)
    , daysOfWeek = fromMaybe [] $ weekDays element
    , specialDaysOfOperation = fromMaybe [] $ operationDays element
    , specialDaysOfNonOperation = fromMaybe [] $ nonOperationDays element
    , note = maybe ""
                   strContent
                   (findChild "Note" element >>= findChild "NoteText")
    , noteCode = maybe ""
                       strContent
                       (findChild "Note" element >>= findChild "NoteCode")
    }

  nonOperationDays :: Element -> Maybe [DateRange]
  nonOperationDays element = do
    operatingProfile     <- findChild "OperatingProfile" element
    specialDaysOperation <- findChild "SpecialDaysOperation" operatingProfile
    daysOfNonOperation   <- findChild "DaysOfNonOperation" specialDaysOperation
    return $ elementToDateRange <$> findChildren "DateRange" daysOfNonOperation

  operationDays :: Element -> Maybe [DateRange]
  operationDays element = do
    operatingProfile     <- findChild "OperatingProfile" element
    specialDaysOperation <- findChild "SpecialDaysOperation" operatingProfile
    daysOfOperation      <- findChild "DaysOfOperation" specialDaysOperation
    return $ elementToDateRange <$> findChildren "DateRange" daysOfOperation

  weekDays :: Element -> Maybe [WeekDay]
  weekDays element = do
    operatingProfile  <- findChild "OperatingProfile" element
    regularDayType    <- findChild "RegularDayType" operatingProfile
    daysOfWeekElement <- findChild "DaysOfWeek" regularDayType
    return $ elementToDay <$> elChildren daysOfWeekElement

  elementToDay :: Element -> TransxchangeTypes.WeekDay
  elementToDay element = case qName $ elName element of
    "Monday"    -> TransxchangeTypes.Monday
    "Tuesday"   -> TransxchangeTypes.Tuesday
    "Wednesday" -> TransxchangeTypes.Wednesday
    "Thursday"  -> TransxchangeTypes.Thursday
    "Friday"    -> TransxchangeTypes.Friday
    "Saturday"  -> TransxchangeTypes.Saturday
    "Sunday"    -> TransxchangeTypes.Sunday

elementToDateRange :: Element -> DateRange
elementToDateRange element = DateRange
  { startDate = parseXMLDate . strContent <$> findChild "StartDate" element
  , endDate   = parseXMLDate . strContent <$> findChild "EndDate" element
  }
