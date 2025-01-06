{-# LANGUAGE OverloadedStrings #-}

module TransxchangeParser
  ( parseTransxchangeXML,
  )
where

import Data.Maybe
  ( fromJust,
    fromMaybe,
    isJust,
  )
import Data.String (IsString (..))
import Data.Time.Calendar
  ( Day,
    fromGregorian,
  )
import Text.XML.Light
  ( Element (elName),
    QName (QName, qName),
    elChildren,
    findAttr,
    findChild,
    findChildren,
    onlyElems,
    parseXML,
    strContent,
  )
import TransxchangeTypes
import Utility (stringToDay)

parseTransxchangeXML :: String -> TransXChangeData
parseTransxchangeXML input =
  TransXChangeData
    { stopPoints = fromMaybe [] $ getStopPoints transXChangeElement,
      servicedOrganisations =
        fromMaybe [] $
          getServicedOrganisations transXChangeElement,
      routeSections =
        fromMaybe [] $
          getRouteSections serviceCode' transXChangeElement,
      routes =
        fromMaybe [] $
          getRoutes serviceCode' transXChangeElement,
      journeyPatternSections =
        fromMaybe [] $
          getJourneyPatternSections serviceCode' transXChangeElement,
      operators =
        fromMaybe [] $
          getOperators transXChangeElement,
      services = services,
      vehicleJourneys =
        fromMaybe [] $
          getVehicleJourneys serviceCode' transXChangeElement
    }
  where
    xml = onlyElems $ parseXML input
    transXChangeElement = xml !! 1
    services = fromMaybe [] $ getServices transXChangeElement
    serviceCode' = serviceCode $ head services

-- Helpers
instance IsString QName where
  fromString name = QName name (Just "http://www.transxchange.org.uk/") Nothing

attr :: String -> QName
attr name = QName name Nothing Nothing

attrValue :: String -> Element -> String
attrValue name element = fromMaybe "" $ findAttr (attr name) element

-- Stop Points
getStopPoints :: Element -> Maybe [AnnotatedStopPointRef]
getStopPoints element = do
  stopPointElement <- findChild "StopPoints" element
  let stopPointElements = findChildren "AnnotatedStopPointRef" stopPointElement
  return $ elementToStopPoint <$> stopPointElements
  where
    elementToStopPoint :: Element -> AnnotatedStopPointRef
    elementToStopPoint stopPointElement =
      AnnotatedStopPointRef
        { stopPointRef =
            maybe
              ""
              strContent
              (findChild "StopPointRef" stopPointElement),
          commonName = maybe "" strContent (findChild "CommonName" stopPointElement)
        }

-- Serviced organisations
getServicedOrganisations :: Element -> Maybe [ServicedOrganisation]
getServicedOrganisations element = do
  servicedOrganisationsElement <- findChild "ServicedOrganisations" element
  let servicedOrganisationElements =
        findChildren "ServicedOrganisation" servicedOrganisationsElement
  return $ elementToServicedOrganisation <$> servicedOrganisationElements
  where
    elementToServicedOrganisation :: Element -> ServicedOrganisation
    elementToServicedOrganisation element =
      ServicedOrganisation
        { organisationCode =
            maybe
              ""
              strContent
              (findChild "OrganisationCode" element),
          organisationName = maybe "" strContent (findChild "Name" element),
          organisationWorkingDays = fromMaybe [] $ dateRangeElements element
        }

    dateRangeElements :: Element -> Maybe [DateRange]
    dateRangeElements element = do
      workingDays <- findChild "WorkingDays" element
      let dateRangeElements = findChildren "DateRange" workingDays
      return $ elementToDateRange <$> dateRangeElements

-- Route Sections
getRouteSections :: String -> Element -> Maybe [RouteSection]
getRouteSections serviceCode element = do
  routeSectionsElement <- findChild "RouteSections" element
  let routeSectionElements = findChildren "RouteSection" routeSectionsElement
  return $ elementToRouteSection <$> routeSectionElements
  where
    elementToRouteSection :: Element -> RouteSection
    elementToRouteSection element =
      RouteSection
        { routeSectionId = attrValue "id" element <> "-" <> serviceCode,
          routeLinks = elementToRouteLink <$> findChildren "RouteLink" element
        }

    elementToRouteLink :: Element -> RouteLink
    elementToRouteLink element =
      RouteLink
        { routeLinkId = attrValue "id" element <> "-" <> serviceCode,
          fromStopPointRef =
            maybe
              ""
              strContent
              ( findChild "From" element
                  >>= findChild "StopPointRef"
              ),
          toStopPointRef =
            maybe
              ""
              strContent
              (findChild "To" element >>= findChild "StopPointRef"),
          routeDirection = maybe "" strContent (findChild "Direction" element)
        }

-- Routes
getRoutes :: String -> Element -> Maybe [Route]
getRoutes serviceCode element = do
  routesElement <- findChild "Routes" element
  let routeElements = findChildren "Route" routesElement
  return $ elementToRoute <$> routeElements
  where
    elementToRoute :: Element -> Route
    elementToRoute element =
      Route
        { routeId = attrValue "id" element <> "-" <> serviceCode,
          routeDescription = maybe "" strContent (findChild "Description" element),
          routeSectionRef =
            maybe
              ""
              (\e -> strContent e <> "-" <> serviceCode)
              (findChild "RouteSectionRef" element)
        }

-- JourneyPatternSection
getJourneyPatternSections :: String -> Element -> Maybe [JourneyPatternSection]
getJourneyPatternSections serviceCode element = do
  journeyPatternSectionsElement <- findChild "JourneyPatternSections" element
  let journeyPatternSectionsElements =
        findChildren "JourneyPatternSection" journeyPatternSectionsElement
  return $ elementToJourneyPatternSection <$> journeyPatternSectionsElements
  where
    elementToJourneyPatternSection :: Element -> JourneyPatternSection
    elementToJourneyPatternSection element =
      JourneyPatternSection
        { journeyPatterSectionId = attrValue "id" element <> "-" <> serviceCode,
          journeyPatternTimingLinks =
            elementToJourneyPatternTimingLink
              <$> findChildren
                "JourneyPatternTimingLink"
                element
        }

    elementToJourneyPatternTimingLink :: Element -> JourneyPatternTimingLink
    elementToJourneyPatternTimingLink element =
      JourneyPatternTimingLink
        { journeyPatternTimingLinkId = attrValue "id" element <> "-" <> serviceCode,
          journeyPatternFromWaitTime =
            maybe
              ""
              strContent
              ( findChild "From" element
                  >>= findChild "WaitTime"
              ),
          journeyPatternFromStopPointRef =
            maybe
              ""
              strContent
              ( findChild "From" element
                  >>= findChild "StopPointRef"
              ),
          journeyPatternFromTimingStatus =
            maybe
              ""
              strContent
              ( findChild "From" element
                  >>= findChild "TimingStatus"
              ),
          journeyPatternToStopPointsRef =
            maybe
              ""
              strContent
              ( findChild "To" element
                  >>= findChild "StopPointRef"
              ),
          journeyPatternToTimingStatus =
            maybe
              ""
              strContent
              ( findChild "To" element
                  >>= findChild "TimingStatus"
              ),
          routeLinkRef = maybe "" (\e -> strContent e <> "-" <> serviceCode) (findChild "RouteLinkRef" element),
          journeyDirection = maybe "" strContent (findChild "Direction" element),
          runTime = maybe "" strContent (findChild "RunTime" element)
        }

-- Operators
getOperators :: Element -> Maybe [Operator]
getOperators element = do
  operatorsElement <- findChild "Operators" element
  let operatorsElements = findChildren "Operator" operatorsElement
  return $ elementToOperator <$> operatorsElements
  where
    elementToOperator :: Element -> Operator
    elementToOperator element =
      Operator
        { operatorId = attrValue "id" element,
          nationalOperatorCode =
            maybe
              ""
              strContent
              (findChild "NationalOperatorCode" element),
          operatorCode = maybe "" strContent (findChild "OperatorCode" element),
          operatorShortName =
            maybe
              ""
              strContent
              (findChild "OperatorShortName" element)
        }

-- Services
getServices :: Element -> Maybe [Service]
getServices element = do
  servicesElement <- findChild "Services" element
  let serviceElements = findChildren "Service" servicesElement
  let serviceCode = getServiceCode $ head serviceElements
  return $ elementToService serviceCode <$> serviceElements
  where
    getServiceCode :: Element -> String
    getServiceCode element = maybe "" strContent (findChild "ServiceCode" element)

    elementToService :: String -> Element -> Service
    elementToService serviceCode element =
      Service
        { serviceCode = serviceCode,
          TransxchangeTypes.lines = fromMaybe [] $ getLines serviceCode element,
          operatingPeriod =
            maybe
              (DateRange Nothing Nothing)
              elementToDateRange
              (findChild "OperatingPeriod" element),
          registeredOperatorRef =
            maybe
              ""
              strContent
              (findChild "RegisteredOperatorRef" element),
          mode = maybe "" strContent (findChild "Mode" element),
          description = maybe "" strContent (findChild "Description" element),
          standardService =
            maybe
              (StandardService "" "" [])
              (elementToStandardService serviceCode)
              (findChild "StandardService" element)
        }

-- Lines
getLines :: String -> Element -> Maybe [TransxchangeTypes.Line]
getLines serviceCode element = do
  linesElement <- findChild "Lines" element
  let lineElements = findChildren "Line" linesElement
  return $ elementToLine <$> lineElements
  where
    elementToLine :: Element -> TransxchangeTypes.Line
    elementToLine element =
      TransxchangeTypes.Line
        { lineId = attrValue "id" element <> "-" <> serviceCode,
          lineName = maybe "" strContent (findChild "LineName" element)
        }

-- Standard Service
elementToStandardService :: String -> Element -> StandardService
elementToStandardService serviceCode element =
  StandardService
    { origin = maybe "" strContent (findChild "Origin" element),
      destination = maybe "" strContent (findChild "Destination" element),
      journeyPatterns =
        elementToJourneyPattern
          <$> findChildren "JourneyPattern" element
    }
  where
    elementToJourneyPattern :: Element -> JourneyPattern
    elementToJourneyPattern element =
      JourneyPattern
        { journeyPatternId = attrValue "id" element <> "-" <> serviceCode,
          journeyPatternDirection =
            maybe
              ""
              strContent
              (findChild "Direction" element),
          journeyPatternSectionRef =
            maybe
              ""
              (\e -> strContent e <> "-" <> serviceCode)
              ( findChild
                  "JourneyPatternSectionRefs"
                  element
              )
        }

-- Vehicle journeys
getVehicleJourneys :: String -> Element -> Maybe [VehicleJourney]
getVehicleJourneys serviceCode element = do
  vehicleJourneysElement <- findChild "VehicleJourneys" element
  let vehicleJourneyElements =
        findChildren "VehicleJourney" vehicleJourneysElement
  return $ elementToVehicleJourney <$> vehicleJourneyElements
  where
    elementToVehicleJourney :: Element -> VehicleJourney
    elementToVehicleJourney element =
      VehicleJourney
        { operatorRef = maybe "" strContent (findChild "OperatorRef" element),
          vehicleJourneyCode =
            maybe
              ""
              (\e -> strContent e <> "-" <> serviceCode)
              (findChild "VehicleJourneyCode" element),
          serviceRef = maybe "" strContent (findChild "ServiceRef" element),
          lineRef =
            maybe
              ""
              (\e -> strContent e <> "-" <> serviceCode)
              (findChild "LineRef" element),
          journeyPatternRef =
            maybe
              ""
              (\e -> strContent e <> "-" <> serviceCode)
              (findChild "JourneyPatternRef" element),
          departureTime = maybe "" strContent (findChild "DepartureTime" element),
          daysOfWeek = fromMaybe [] $ weekDays element,
          specialDaysOfOperation = fromMaybe [] $ operationDays element,
          specialDaysOfNonOperation = fromMaybe [] $ nonOperationDays element,
          note =
            maybe
              ""
              strContent
              (findChild "Note" element >>= findChild "NoteText"),
          noteCode =
            maybe
              ""
              strContent
              (findChild "Note" element >>= findChild "NoteCode"),
          daysOfNonOperationServicedOrganisationRef =
            strContent
              <$> ( findChild "OperatingProfile" element
                      >>= findChild "ServicedOrganisationDayType"
                      >>= findChild "DaysOfNonOperation"
                      >>= findChild "WorkingDays"
                      >>= findChild "ServicedOrganisationRef"
                  )
        }

    nonOperationDays :: Element -> Maybe [DateRange]
    nonOperationDays element = do
      operatingProfile <- findChild "OperatingProfile" element
      specialDaysOperation <- findChild "SpecialDaysOperation" operatingProfile
      daysOfNonOperation <- findChild "DaysOfNonOperation" specialDaysOperation
      return $ elementToDateRange <$> findChildren "DateRange" daysOfNonOperation

    operationDays :: Element -> Maybe [DateRange]
    operationDays element = do
      operatingProfile <- findChild "OperatingProfile" element
      specialDaysOperation <- findChild "SpecialDaysOperation" operatingProfile
      daysOfOperation <- findChild "DaysOfOperation" specialDaysOperation
      return $ elementToDateRange <$> findChildren "DateRange" daysOfOperation

    weekDays :: Element -> Maybe [WeekDay]
    weekDays element = do
      operatingProfile <- findChild "OperatingProfile" element
      regularDayType <- findChild "RegularDayType" operatingProfile
      if isJust $ findChild "HolidaysOnly" regularDayType
        then return [TransxchangeTypes.HolidaysOnly]
        else do
          daysOfWeekElement <- findChild "DaysOfWeek" regularDayType
          return $ concatMap elementToDays (elChildren daysOfWeekElement)

    elementToDays :: Element -> [TransxchangeTypes.WeekDay]
    elementToDays element =
      let elementName = qName $ elName element
       in case elementName of
            "Monday" -> [TransxchangeTypes.Monday]
            "Tuesday" -> [TransxchangeTypes.Tuesday]
            "Wednesday" -> [TransxchangeTypes.Wednesday]
            "Thursday" -> [TransxchangeTypes.Thursday]
            "Friday" -> [TransxchangeTypes.Friday]
            "Saturday" -> [TransxchangeTypes.Saturday]
            "Sunday" -> [TransxchangeTypes.Sunday]
            "MondayToFriday" ->
              [ TransxchangeTypes.Monday,
                TransxchangeTypes.Tuesday,
                TransxchangeTypes.Wednesday,
                TransxchangeTypes.Thursday,
                TransxchangeTypes.Friday
              ]
            _ -> fail $ "Unrecognised day of week element: " <> elementName

elementToDateRange :: Element -> DateRange
elementToDateRange element =
  DateRange
    { startDate =
        fromJust
          . stringToDay
          . strContent
          <$> findChild "StartDate" element,
      endDate =
        fromJust
          . stringToDay
          . strContent
          <$> findChild "EndDate" element
    }