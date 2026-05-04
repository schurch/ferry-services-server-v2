{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module WebServer where

import Control.Lens ((&), (.~), (?~), (%~), (^.), _Just, traversed)
import Control.Monad (forM)
import Control.Monad.Reader
  ( ReaderT,
    ask,
    runReaderT,
  )
import Control.Monad.IO.Class (liftIO)
import qualified App.Env as App
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import qualified Data.Char as Char
import Data.Aeson (decode, encode)
import Data.Default (def)
import Data.List (find, sortOn, stripPrefix)
import qualified Data.Map as M
import Data.Maybe
  ( fromMaybe,
  )
import qualified Crypto.Hash as Crypto
import Data.Proxy (Proxy (Proxy))
import Data.Scientific
  ( Scientific (..),
    fromFloatDigits,
    toRealFloat,
  )
import qualified Data.Set as S
import qualified Data.OpenApi as OpenApi
import Data.Text (Text)
import Data.Time
  ( LocalTime,
    UTCTime (UTCTime),
    diffUTCTime,
    getCurrentTime,
    localTimeToUTC,
    utctDay,
    defaultTimeLocale,
    formatTime,
  )
import Data.Time.Calendar (Day)
import Data.UUID (UUID, fromText)
import qualified Database as DB
import Database.Postgis
  ( Geometry (GeoPoint),
    Point (Point),
    Position (Position),
  )
import qualified Network.Wai
import Network.Wai (Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Gzip
  ( GzipFiles (..),
    gzip,
    gzipFiles,
  )
import Network.Wai.Middleware.RequestLogger
  ( Destination (Callback),
    IPAddrSource (FromFallback),
    OutputFormat (Apache),
    RequestLoggerSettings (destination, outputFormat),
  )
import Network.Wai.Middleware.Static
  ( addBase,
    isNotAbsolute,
    noDots,
    staticPolicy,
  )
import System.Log.FastLogger.Internal (LogStr, fromLogStr)
import App.Logger
  ( Level (Debug, Info, Trace),
    Logger,
    level,
    logMessage,
  )
import Types
import Types.Api
import Utility (stringToDay)
import qualified Push
import OfflineSnapshot
  ( OfflineSnapshot,
    OfflineSnapshotMetadata (offlineSnapshotMetadataEtag),
    defaultSnapshotMetadataPath,
    defaultSnapshotPath,
  )
import Servant
  ( (:>),
    (:<|>) (..),
    Accept (contentType),
    Capture,
    Delete,
    Get,
    Handler,
    Header,
    Headers,
    JSON,
    MimeRender (mimeRender),
    Post,
    QueryParam,
    ReqBody,
    ServerT,
    Description,
    Summary,
    addHeader,
    err400,
    err304,
    err404,
    err503,
    errHeaders,
    hoistServer,
    serve,
    throwError,
  )
import Servant.OpenApi (toOpenApi)
import System.Directory (doesFileExist, getModificationTime)

data HTML

instance Accept HTML where
  contentType _ = "text/html;charset=utf-8"

instance MimeRender HTML BL.ByteString where
  mimeRender _ = id

data JavaScript

instance Accept JavaScript where
  contentType _ = "application/javascript;charset=utf-8"

instance MimeRender JavaScript BL.ByteString where
  mimeRender _ = id

data SnapshotJSON

instance Accept SnapshotJSON where
  contentType _ = "application/json;charset=utf-8"

newtype SnapshotBody = SnapshotBody BL.ByteString

instance MimeRender SnapshotJSON SnapshotBody where
  mimeRender _ (SnapshotBody body) = body

instance OpenApi.ToSchema SnapshotBody where
  declareNamedSchema _ = OpenApi.declareNamedSchema (Proxy :: Proxy OfflineSnapshot)

type API =
  DocumentationAPI :<|> AppAPI

type DocumentationAPI =
  "openapi.json" :> Get '[JSON] OpenApi.OpenApi
    :<|> "swagger" :> Get '[HTML] BL.ByteString
    :<|> "swagger-initializer.js" :> Get '[JavaScript] BL.ByteString

type AppAPI =
  Get '[HTML] BL.ByteString
    :<|> JsonAPI

type JsonAPI =
  Summary "List services" :> Description "Returns all visible ferry services with current live status, operator, route and location metadata. Scheduled departures are not embedded in this list response." :> "api" :> "services" :> Get '[JSON] [ServiceResponse]
    :<|> Summary "Get service detail" :> Description "Returns one visible service. Pass departuresDate as YYYY-MM-DD to include scheduled ferry departures for that local date." :> "api" :> "services" :> Capture "serviceID" Int :> QueryParam "departuresDate" String :> Get '[JSON] (Maybe ServiceResponse)
    :<|> Summary "Create installation" :> Description "Registers a mobile app installation for push notifications and returns that installation's saved services." :> "api" :> "installations" :> Capture "installationID" Text :> ReqBody '[JSON] CreateInstallationRequest :> Post '[JSON] [ServiceResponse]
    :<|> Summary "Get push status" :> Description "Returns whether push notifications are enabled for the mobile app installation." :> "api" :> "installations" :> Capture "installationID" Text :> "push-status" :> Get '[JSON] PushStatus
    :<|> Summary "Update push status" :> Description "Enables or disables push notifications for the mobile app installation." :> "api" :> "installations" :> Capture "installationID" Text :> "push-status" :> ReqBody '[JSON] PushStatus :> Post '[JSON] PushStatus
    :<|> Summary "List installation services" :> Description "Returns the services saved by one mobile app installation." :> "api" :> "installations" :> Capture "installationID" Text :> "services" :> Get '[JSON] [ServiceResponse]
    :<|> Summary "Add installation service" :> Description "Adds a service to one mobile app installation and returns the updated saved service list." :> "api" :> "installations" :> Capture "installationID" Text :> "services" :> ReqBody '[JSON] AddServiceRequest :> Post '[JSON] [ServiceResponse]
    :<|> Summary "Delete installation service" :> Description "Removes a service from one mobile app installation and returns the updated saved service list." :> "api" :> "installations" :> Capture "installationID" Text :> "services" :> Capture "serviceID" Int :> Delete '[JSON] [ServiceResponse]
    :<|> Summary "List vessels" :> Description "Returns recent vessel positions used by the live service UI." :> "api" :> "vessels" :> Get '[JSON] [VesselResponse]
    :<|> Summary "List timetable documents" :> Description "Returns current operator timetable documents. Pass serviceID to filter to documents linked to one service; omit it for the global timetable downloads screen. Clients should send If-None-Match with the stored ETag; unchanged lists return 304 Not Modified." :> "api" :> "timetable-documents" :> QueryParam "serviceID" Int :> Header "If-None-Match" String :> Get '[JSON] (Headers '[Header "Cache-Control" String, Header "ETag" String] [TimetableDocumentResponse])
    :<|> Summary "Download offline snapshot" :> Description "Returns the generated offline timetable snapshot. Clients should send If-None-Match with the stored ETag; unchanged snapshots return 304 Not Modified. The response is CDN-cacheable and includes Cache-Control, ETag and Last-Modified headers." :> "api" :> "offline" :> "snapshot.json" :> Header "If-None-Match" String :> Get '[SnapshotJSON] (Headers '[Header "Cache-Control" String, Header "ETag" String, Header "Last-Modified" String] SnapshotBody)

api :: Proxy API
api = Proxy

type WebHandler = ReaderT App.Env Handler

webApp :: App.Env -> Middleware -> Network.Wai.Application
webApp env requestLogger =
  middlewares $ serve api $ hoistServer api (runWebHandler env) server
  where
    corsOrigins = ["https://scottishferryapp.com", "https://www.scottishferryapp.com", "http://localhost:3000"]

    middlewares :: Middleware
    middlewares =
      requestLogger
        . cors (const $ Just simpleCorsResourcePolicy {corsOrigins = Just (corsOrigins, False)})
        . gzip def {gzipFiles = GzipCompress}
        . addHeaders
          [ ("X-Frame-Options", "DENY"),
            ("X-Content-Type-Options", "nosniff"),
            ("Content-Security-Policy", "script-src 'self' https:, object-src 'none'; base-uri 'none';")
          ]
        . staticPolicy (noDots <> isNotAbsolute <> addBase "public")

server :: ServerT API WebHandler
server =
  documentationServer
    :<|> appServer

documentationServer :: ServerT DocumentationAPI WebHandler
documentationServer =
  pure openApiSpec
    :<|> pure swaggerHtml
    :<|> pure swaggerInitializer

appServer :: ServerT AppAPI WebHandler
appServer =
  getIndex
    :<|> jsonServer

jsonServer :: ServerT JsonAPI WebHandler
jsonServer =
  getServices
    :<|> getServiceWithDate
    :<|> createInstallationEndpoint
    :<|> getPushStatusEndpoint
    :<|> updatePushStatusEndpoint
    :<|> getServicesForInstallationEndpoint
    :<|> addServiceToInstallationEndpoint
    :<|> deleteServiceForInstallationEndpoint
    :<|> getVessels
    :<|> getTimetableDocuments
    :<|> getOfflineSnapshot

openApiSpec :: OpenApi.OpenApi
openApiSpec =
  normalizeOpenApi $
    OpenApi.applyTags [OpenApi.Tag "Ferry Services API" (Just "Live ferry service data, mobile installation state, vessel positions and offline timetable downloads.") Nothing] $
      toOpenApi (Proxy :: Proxy JsonAPI)
      & OpenApi.info . OpenApi.title .~ "Scottish Ferry Services API"
      & OpenApi.info . OpenApi.version .~ "1.0"

normalizeOpenApi :: OpenApi.OpenApi -> OpenApi.OpenApi
normalizeOpenApi =
  (OpenApi.components . OpenApi.schemas . traversed %~ normalizeSchema)
    . (OpenApi.paths . traversed %~ normalizePathItem)

normalizePathItem :: OpenApi.PathItem -> OpenApi.PathItem
normalizePathItem pathItem =
  pathItem
    & OpenApi.parameters . traversed %~ fmap normalizeParam
    & OpenApi.get . _Just %~ normalizeOperation
    & OpenApi.post . _Just %~ normalizeOperation
    & OpenApi.delete . _Just %~ normalizeOperation

normalizeOperation :: OpenApi.Operation -> OpenApi.Operation
normalizeOperation operation =
  operation
    & OpenApi.parameters . traversed %~ fmap normalizeParam

normalizeParam :: OpenApi.Param -> OpenApi.Param
normalizeParam param =
  if param ^. OpenApi.name == "departuresDate"
    then param & OpenApi.schema . _Just %~ fmap dateSchema
    else param & OpenApi.schema . _Just %~ fmap normalizeSchema

normalizeSchema :: OpenApi.Schema -> OpenApi.Schema
normalizeSchema schema =
  schema
    & OpenApi.format %~ normalizeFormat
    & OpenApi.properties . traversed %~ fmap normalizeSchema
    & OpenApi.items . _Just %~ normalizeItems
    & OpenApi.allOf . _Just . traversed %~ fmap normalizeSchema
    & OpenApi.oneOf . _Just . traversed %~ fmap normalizeSchema
    & OpenApi.anyOf . _Just . traversed %~ fmap normalizeSchema
    & OpenApi.not_ . _Just %~ fmap normalizeSchema

normalizeItems :: OpenApi.OpenApiItems -> OpenApi.OpenApiItems
normalizeItems (OpenApi.OpenApiItemsObject itemSchema) =
  OpenApi.OpenApiItemsObject (normalizeSchema <$> itemSchema)
normalizeItems (OpenApi.OpenApiItemsArray itemSchemas) =
  OpenApi.OpenApiItemsArray (fmap (fmap normalizeSchema) itemSchemas)

normalizeFormat :: Maybe OpenApi.Format -> Maybe OpenApi.Format
normalizeFormat (Just "yyyy-mm-ddThh:MM:ssZ") = Just "date-time"
normalizeFormat value = value

dateSchema :: OpenApi.Schema -> OpenApi.Schema
dateSchema schema =
  schema
    & OpenApi.type_ ?~ OpenApi.OpenApiString
    & OpenApi.format ?~ "date"

swaggerHtml :: BL.ByteString
swaggerHtml =
  "<!doctype html><html><head><meta charset=\"utf-8\"><title>Scottish Ferry Services API</title><link rel=\"stylesheet\" href=\"https://unpkg.com/swagger-ui-dist@5/swagger-ui.css\"></head><body><div id=\"swagger-ui\"></div><script src=\"https://unpkg.com/swagger-ui-dist@5/swagger-ui-bundle.js\"></script><script src=\"/swagger-initializer.js\"></script></body></html>"

swaggerInitializer :: BL.ByteString
swaggerInitializer =
  "window.onload = function () { window.ui = SwaggerUIBundle({ url: '/openapi.json', dom_id: '#swagger-ui' }); };"

runWebHandler :: App.Env -> WebHandler a -> Handler a
runWebHandler env action = runReaderT action env

runApplication :: App.Application a -> WebHandler a
runApplication action = do
  env <- ask
  liftIO $ runReaderT action env

getIndex :: WebHandler BL.ByteString
getIndex = liftIO $ BL.readFile "public/index.html"

parseUUID :: Text -> WebHandler UUID
parseUUID value =
  maybe (throwError err400) return $ fromText value

notFound :: Maybe a -> WebHandler a
notFound = maybe (throwError err404) return

getServiceWithDate :: Int -> Maybe String -> WebHandler (Maybe ServiceResponse)
getServiceWithDate serviceID departuresDateParam =
  getService serviceID (departuresDateParam >>= stringToDay)

createInstallationEndpoint :: Text -> CreateInstallationRequest -> WebHandler [ServiceResponse]
createInstallationEndpoint installationID request = do
  uuid <- parseUUID installationID
  createInstallation uuid request

getPushStatusEndpoint :: Text -> WebHandler PushStatus
getPushStatusEndpoint installationID = do
  uuid <- parseUUID installationID
  getPushStatus uuid >>= notFound

updatePushStatusEndpoint :: Text -> PushStatus -> WebHandler PushStatus
updatePushStatusEndpoint installationID request = do
  uuid <- parseUUID installationID
  runApplication $ DB.updatePushEnabled uuid (pushStatusEnabled request)
  getPushStatus uuid >>= notFound

getServicesForInstallationEndpoint :: Text -> WebHandler [ServiceResponse]
getServicesForInstallationEndpoint installationID = do
  uuid <- parseUUID installationID
  getServicesForInstallation uuid

addServiceToInstallationEndpoint :: Text -> AddServiceRequest -> WebHandler [ServiceResponse]
addServiceToInstallationEndpoint installationID (AddServiceRequest serviceID) = do
  uuid <- parseUUID installationID
  addServiceToInstallation uuid serviceID

deleteServiceForInstallationEndpoint :: Text -> Int -> WebHandler [ServiceResponse]
deleteServiceForInstallationEndpoint installationID serviceID = do
  uuid <- parseUUID installationID
  deleteServiceForInstallation uuid serviceID

loggerSettings :: Logger -> RequestLoggerSettings
loggerSettings logger = case level logger of
  Trace -> debugSettings
  Debug -> debugSettings
  _ -> infoSettings
  where
    debugSettings :: RequestLoggerSettings
    debugSettings = def {destination = Callback (callbackLog Debug)}

    infoSettings :: RequestLoggerSettings
    infoSettings =
      def {outputFormat = Apache FromFallback, destination = Callback (callbackLog Info)}

    callbackLog :: Level -> LogStr -> IO ()
    callbackLog level str =
      logMessage logger level $ removeTrailingNewline $ fromLogStr str

    removeTrailingNewline :: BS.ByteString -> BS.ByteString
    removeTrailingNewline =
      BS.reverse . BS.dropWhile (== fromIntegral (ord '\n')) . BS.reverse

type ServiceLocationLookup = M.Map Int [LocationResponse]

type ServiceVesselLookup = M.Map Int [VesselResponse]

type ServiceOrganisationLookup = M.Map Int OrganisationResponse

type ServiceScheduledDeparturesLookup = S.Set Int

type ServiceTimetableDocumentLookup = M.Map Int [TimetableDocumentResponse]

type LocationScheduledDeparturesLookup = M.Map Int [DepartureResponse]

type LocationNextDepartureLookup = M.Map Int DepartureResponse

type LocationNextRailDepartureLookup = M.Map Int RailDepartureResponse

getPushStatus :: UUID -> WebHandler (Maybe PushStatus)
getPushStatus installationID = do
  installation <- runApplication $ DB.getInstallationWithID installationID
  return $ PushStatus . installationPushEnabled <$> installation

getService :: Int -> Maybe Day -> WebHandler (Maybe ServiceResponse)
getService serviceID departuresDate = do
  service <- runApplication $ DB.getService serviceID
  time <- liftIO getCurrentTime
  locationDepartureLookup <- createDeparturesLookup serviceID departuresDate
  hasScheduledDepartures <- runApplication $ DB.getServiceHasScheduledDeparturesV2 serviceID
  nextDepatureLookup <- createNextDepartureLookup serviceID
  nextRailDepartureLookup <- createNextRailDepartureLookup
  locationLookup <- createLocationLookup (Just locationDepartureLookup) (Just nextDepatureLookup) (Just nextRailDepartureLookup)
  vesselLookup <- createServiceVesselLookup
  organisationLookup <- createServiceOrganisationLookup
  timetableDocumentLookup <- createServiceTimetableDocumentLookup
  let scheduledDeparturesLookup =
        if hasScheduledDepartures
          then S.singleton serviceID
          else S.empty
  return $ serviceToServiceResponse scheduledDeparturesLookup (Just timetableDocumentLookup) vesselLookup locationLookup organisationLookup 1 time <$> service

getServices :: WebHandler [ServiceResponse]
getServices = do
  services <- runApplication DB.getServices
  time <- liftIO getCurrentTime
  scheduledDeparturesLookup <- createServiceScheduledDeparturesLookup
  locationLookup <- createLocationLookup Nothing Nothing Nothing
  vesselLookup <- createServiceVesselLookup
  organisationLookup <- createServiceOrganisationLookup
  forM (zip [1 ..] services) $ \(sortOrder, service) ->
    return $ serviceToServiceResponse scheduledDeparturesLookup Nothing vesselLookup locationLookup organisationLookup sortOrder time service

createInstallation ::
  UUID -> CreateInstallationRequest -> WebHandler [ServiceResponse]
createInstallation installationID (CreateInstallationRequest deviceToken deviceType) =
  do
    awsSNSEndpointARN <-
      runApplication $
      Push.registerDeviceToken
        installationID
        deviceToken
        deviceType
    time <- liftIO getCurrentTime
    runApplication $
      DB.createInstallation
        installationID
        deviceToken
        deviceType
        awsSNSEndpointARN
        time
    getServicesForInstallation installationID

addServiceToInstallation :: UUID -> Int -> WebHandler [ServiceResponse]
addServiceToInstallation installationID serviceID = do
  runApplication $ DB.addServiceToInstallation installationID serviceID
  getServicesForInstallation installationID

deleteServiceForInstallation :: UUID -> Int -> WebHandler [ServiceResponse]
deleteServiceForInstallation installationID serviceID = do
  runApplication $ DB.deleteServiceForInstallation installationID serviceID
  getServicesForInstallation installationID

getServicesForInstallation :: UUID -> WebHandler [ServiceResponse]
getServicesForInstallation installationID = do
  services <- runApplication $ DB.getServicesForInstallation installationID
  time <- liftIO getCurrentTime
  scheduledDeparturesLookup <- createServiceScheduledDeparturesLookup
  locationLookup <- createLocationLookup Nothing Nothing Nothing
  vesselLookup <- createServiceVesselLookup
  organisationLookup <- createServiceOrganisationLookup
  forM (zip [1 ..] services) $ \(sortOrder, service) ->
    return $ serviceToServiceResponse scheduledDeparturesLookup Nothing vesselLookup locationLookup organisationLookup sortOrder time service

getVessels :: WebHandler [VesselResponse]
getVessels = do
  time <- liftIO getCurrentTime
  vessels <- filter (vesselFilter time) <$> runApplication DB.getVessels
  return $ vesselToVesselResponse <$> vessels
  where
    vesselFilter :: UTCTime -> Vessel -> Bool
    vesselFilter currentTime Vessel {vesselLastReceived = vesselLastReceived} =
      vesselTimeFilter currentTime vesselLastReceived

getTimetableDocuments ::
  Maybe Int ->
  Maybe String ->
  WebHandler (Headers '[Header "Cache-Control" String, Header "ETag" String] [TimetableDocumentResponse])
getTimetableDocuments serviceID ifNoneMatch = do
  documents <- runApplication $ DB.getTimetableDocuments serviceID
  serviceLinks <- runApplication DB.getTimetableDocumentServiceLinks
  let serviceIDsByDocumentID =
        M.fromListWith (++) [(documentID, [linkedServiceID]) | (documentID, linkedServiceID) <- serviceLinks]
      response = timetableDocumentToResponse serviceIDsByDocumentID <$> documents
      cacheControl = "private, no-cache, no-transform"
      etag = quoteETag $ responseHash response
      responseHeaders =
        [ ("Cache-Control", BSC.pack cacheControl),
          ("ETag", BSC.pack etag)
        ]
  case ifNoneMatch of
    Just value | etagMatches etag value ->
      throwError err304 {errHeaders = responseHeaders}
    _ ->
      pure $
        addHeader cacheControl $
          addHeader etag response

vesselToVesselResponse :: Vessel -> VesselResponse
vesselToVesselResponse Vessel {..} =
  VesselResponse
    { vesselResponseMmsi = vesselMmsi,
      vesselResponseName = vesselName,
      vesselResponseSpeed = vesselSpeed,
      vesselResponseCourse = vesselCourse,
      vesselResponseLatitude = getLatitude vesselCoordinate,
      vesselResponseLongitude = getLongitude vesselCoordinate,
      vesselResponseLastReceived = vesselLastReceived
    }

serviceToServiceResponse ::
  ServiceScheduledDeparturesLookup -> Maybe ServiceTimetableDocumentLookup -> ServiceVesselLookup -> ServiceLocationLookup -> ServiceOrganisationLookup -> Int -> UTCTime -> Service -> ServiceResponse
serviceToServiceResponse scheduledDeparturesLookup timetableDocumentLookup vesselLookup locationLookup organisationLookup sortOrder currentTime Service {..} =
  ServiceResponse
    { serviceResponseServiceID = serviceID,
      serviceResponseSortOrder = sortOrder,
      serviceResponseArea = serviceArea,
      serviceResponseRoute = serviceRoute,
      serviceResponseStatus =
        serviceStatusForTime
          currentTime
          serviceUpdated
          serviceStatus,
      serviceResponseLocations =
        fromMaybe [] $
          M.lookup serviceID locationLookup,
      serviceResponseAdditionalInfo = serviceAdditionalInfo,
      serviceResponseDisruptionReason = serviceDisruptionReason,
      serviceResponseLastUpdatedDate = serviceLastUpdatedDate,
      serviceResponseVessels =
        fromMaybe [] $
          M.lookup serviceID vesselLookup,
      serviceResponseOperator = M.lookup serviceID organisationLookup,
      serviceResponseScheduledDeparturesAvailable = Just $ S.member serviceID scheduledDeparturesLookup,
      serviceResponseUpdated = serviceUpdated,
      serviceResponseTimetableDocuments = lookupTimetableDocuments
    }
  where
    lookupTimetableDocuments :: Maybe [TimetableDocumentResponse]
    lookupTimetableDocuments =
      fromMaybe [] . M.lookup serviceID <$> timetableDocumentLookup

    -- Unknown status if over 30 mins ago
    serviceStatusForTime :: UTCTime -> UTCTime -> ServiceStatus -> ServiceStatus
    serviceStatusForTime currentTime serviceUpdated serviceStatus =
      let diff = diffUTCTime currentTime serviceUpdated
       in if diff > 1800 then Unknown else serviceStatus

createNextDepartureLookup :: Int -> WebHandler LocationNextDepartureLookup
createNextDepartureLookup serviceID = do
  time <- liftIO getCurrentTime
  departuresLookup <- createDeparturesLookup serviceID (Just $ utctDay time)
  return $ M.mapMaybe (findNextDepature time) departuresLookup
  where
    findNextDepature :: UTCTime -> [DepartureResponse] -> Maybe DepartureResponse
    findNextDepature time = find (\d -> departureResponseDeparture d > time) . sortOn departureResponseDeparture

createNextRailDepartureLookup :: WebHandler LocationNextRailDepartureLookup
createNextRailDepartureLookup = do
  time <- liftIO getCurrentTime
  railDepartures <- runApplication $ DB.getLocationRailDepartures (utctDay time)
  let departuresLookup =
        M.fromListWith (++) $
          [(locationID, [locationRailDepartureToRailDepartureResponse locationRailDeparture]) | locationRailDeparture@(LocationRailDeparture locationID _ _ _ _ _ _ _ _) <- railDepartures]
  return $ M.mapMaybe (findNextDepature time) departuresLookup
  where
    locationRailDepartureToRailDepartureResponse :: LocationRailDeparture -> RailDepartureResponse
    locationRailDepartureToRailDepartureResponse railDeparture =
      RailDepartureResponse
        { railDepartureResponseFrom = locationRailDepartureDepartureName railDeparture,
          railDepartureResponseTo = locationRailDepartureDestinationName railDeparture,
          railDepartureResponseDeparture = convertLocalTimeToUTC $ locationRailDepartureScheduledDepartureTime railDeparture,
          railDepartureResponseDepartureInfo = locationRailDepartureEstimatedDepartureTime railDeparture,
          railDepartureResponsePlatform = locationRailDeparturePlatform railDeparture,
          railDepartureResponseIsCancelled = locationRailDepartureCancelled railDeparture
        }

    findNextDepature :: UTCTime -> [RailDepartureResponse] -> Maybe RailDepartureResponse
    findNextDepature time = find (\d -> railDepartureResponseDeparture d > time) . sortOn railDepartureResponseDeparture

createDeparturesLookup :: Int -> Maybe Day -> WebHandler LocationScheduledDeparturesLookup
createDeparturesLookup serviceID departuresDate = do
  time <- liftIO getCurrentTime
  let date = fromMaybe (utctDay time) departuresDate
  locationDepartures <- runApplication $ DB.getLocationDeparturesV2 serviceID date
  return $
    M.fromListWith (++) $
      [(fromLocationID, [departureResponse locationDeparture]) | locationDeparture@(LocationDeparture fromLocationID _ _ _ _ _ _) <- reverse locationDepartures]
  where
    departureResponse :: LocationDeparture -> DepartureResponse
    departureResponse LocationDeparture {..} =
      DepartureResponse
        { departureResponseDestination =
            LocationResponse
              { locationResponseID = locationDepartureToLocationID,
                locationResponseName = locationDepartureToLocationName,
                locationResponseLatitude = getLatitude locationDepartureToLocationCoordinate,
                locationResponseLongitude = getLongitude locationDepartureToLocationCoordinate,
                locationResponseScheduledDepartures = Nothing,
                locationResponseNextDeparture = Nothing,
                locationResponseNextRailDeparture = Nothing,
                locationResponseWeather = Nothing
              },
          departureResponseDeparture = convertLocalTimeToUTC locationDepartureDepartue,
          departureResponseArrival = convertLocalTimeToUTC locationDepartureArrival,
          departureResponseNotes = locationDepartureNotes
        }

createLocationLookup :: Maybe LocationScheduledDeparturesLookup -> Maybe LocationNextDepartureLookup -> Maybe LocationNextRailDepartureLookup -> WebHandler ServiceLocationLookup
createLocationLookup scheduledDeparturesLookup nextDepatureLookup nextRailDepartureLookup = do
  serviceLocations <- runApplication DB.getServiceLocations
  locationWeatherLookup <- createLocationWeatherLookup
  return $
    M.fromListWith (++) $
      [ ( serviceID,
          [ LocationResponse
              locationID
              name
              (getLatitude coordinate)
              (getLongitude coordinate)
              (lookupDepartures locationID)
              (lookupNextDepature locationID)
              (lookupNextRailDeparture locationID)
              (M.lookup locationID locationWeatherLookup)
          ]
        )
        | (ServiceLocation serviceID locationID name coordinate) <-
            serviceLocations
      ]
  where
    lookupNextDepature :: Int -> Maybe DepartureResponse
    lookupNextDepature locationID = nextDepatureLookup >>= M.lookup locationID

    lookupNextRailDeparture :: Int -> Maybe RailDepartureResponse
    lookupNextRailDeparture locationID = nextRailDepartureLookup >>= M.lookup locationID

    lookupDepartures :: Int -> Maybe [DepartureResponse]
    lookupDepartures locationID = fromMaybe [] . M.lookup locationID <$> scheduledDeparturesLookup

createServiceOrganisationLookup :: WebHandler ServiceOrganisationLookup
createServiceOrganisationLookup = do
  serviceOrganisations <- runApplication DB.getServiceOrganisations
  return $
    M.fromList $
      [ (serviceID, OrganisationResponse organisationID name website localNumber internationalNumber email x facebook)
        | (ServiceOrganisation serviceID organisationID name website localNumber internationalNumber email x facebook) <- serviceOrganisations
      ]

createServiceScheduledDeparturesLookup :: WebHandler ServiceScheduledDeparturesLookup
createServiceScheduledDeparturesLookup =
  S.fromList <$> runApplication DB.getServicesWithScheduledDeparturesV2

createServiceTimetableDocumentLookup :: WebHandler ServiceTimetableDocumentLookup
createServiceTimetableDocumentLookup = do
  documents <- runApplication $ DB.getTimetableDocuments Nothing
  serviceLinks <- runApplication DB.getTimetableDocumentServiceLinks
  let serviceIDsByDocumentID =
        M.fromListWith (++) [(documentID, [serviceID]) | (documentID, serviceID) <- serviceLinks]
      documentResponses =
        M.fromList [(timetableDocumentID document, timetableDocumentToResponse serviceIDsByDocumentID document) | document <- documents]
  return $
    M.fromListWith (++) $
      [ (serviceID, [documentResponse])
        | (documentID, serviceID) <- serviceLinks,
          Just documentResponse <- [M.lookup documentID documentResponses]
      ]

timetableDocumentToResponse :: M.Map Int [Int] -> TimetableDocument -> TimetableDocumentResponse
timetableDocumentToResponse serviceIDsByDocumentID TimetableDocument {..} =
  TimetableDocumentResponse
    { timetableDocumentResponseID = timetableDocumentID,
      timetableDocumentResponseOrganisationID = timetableDocumentOrganisationID,
      timetableDocumentResponseOrganisationName = timetableDocumentOrganisationName,
      timetableDocumentResponseServiceIds = sortOn id $ fromMaybe [] $ M.lookup timetableDocumentID serviceIDsByDocumentID,
      timetableDocumentResponseTitle = timetableDocumentTitle,
      timetableDocumentResponseSourceURL = timetableDocumentSourceURL,
      timetableDocumentResponseContentHash = timetableDocumentContentHash,
      timetableDocumentResponseContentType = timetableDocumentContentType,
      timetableDocumentResponseContentLength = timetableDocumentContentLength,
      timetableDocumentResponseLastSeenAt = timetableDocumentLastSeenAt,
      timetableDocumentResponseUpdated = timetableDocumentUpdated
    }

createServiceVesselLookup :: WebHandler ServiceVesselLookup
createServiceVesselLookup = do
  serviceVessels <- runApplication DB.getServiceVessels
  time <- liftIO getCurrentTime
  return $
    M.fromListWith (++) $
      [ (serviceID, [vesselToVesselResponse (Vessel mmsi name speed course coordinate lastReceived updated organisationID)])
        | vessel@(ServiceVessel serviceID mmsi name speed course coordinate lastReceived updated organisationID) <- serviceVessels,
          serviceVesselFilter time vessel
      ]
  where
    serviceVesselFilter :: UTCTime -> ServiceVessel -> Bool
    serviceVesselFilter currentTime ServiceVessel {serviceVesselLastReceived = vesselLastReceived} =
      vesselTimeFilter currentTime vesselLastReceived

getOfflineSnapshot ::
  Maybe String ->
  WebHandler (Headers '[Header "Cache-Control" String, Header "ETag" String, Header "Last-Modified" String] SnapshotBody)
getOfflineSnapshot ifNoneMatch = do
  snapshotExists <- liftIO $ doesFileExist defaultSnapshotPath
  metadataExists <- liftIO $ doesFileExist defaultSnapshotMetadataPath
  if not snapshotExists
    then throwError err404
    else
      if not metadataExists
        then throwError err503
        else do
          metadataBody <- liftIO $ BL.readFile defaultSnapshotMetadataPath
          metadata <-
            maybe (throwError err503) pure $
              decode metadataBody
          modified <- liftIO $ getModificationTime defaultSnapshotPath
          let cacheControl = "public, max-age=900, stale-while-revalidate=86400"
              etag = offlineSnapshotMetadataEtag metadata
              lastModified = formatHttpDate modified
              responseHeaders =
                [ ("Cache-Control", BSC.pack cacheControl),
                  ("ETag", BSC.pack etag),
                  ("Last-Modified", BSC.pack lastModified)
                ]
          case ifNoneMatch of
            Just value | etagMatches etag value ->
              throwError err304 {errHeaders = responseHeaders}
            _ -> do
              snapshotBody <- liftIO $ BL.readFile defaultSnapshotPath
              pure $
                addHeader cacheControl $
                  addHeader etag $
                    addHeader lastModified (SnapshotBody snapshotBody)
  where
    formatHttpDate =
      formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

responseHash :: [TimetableDocumentResponse] -> String
responseHash response = "sha256-" <> show (Crypto.hashlazy (encode response) :: Crypto.Digest Crypto.SHA256)

quoteETag :: String -> String
quoteETag value = "\"" <> value <> "\""

etagMatches :: String -> String -> Bool
etagMatches current value =
  any matches (splitETags value)
  where
    matches candidate =
      candidate == "*" || stripWeak candidate == stripWeak current

stripWeak :: String -> String
stripWeak value =
  case stripPrefix "W/" (trim value) of
    Just strongValue -> strongValue
    Nothing -> trim value

splitETags :: String -> [String]
splitETags "" = []
splitETags value =
  let (candidate, rest) = break (== ',') value
   in trim candidate :
        case rest of
          [] -> []
          (_ : remaining) -> splitETags remaining

trim :: String -> String
trim = dropWhile Char.isSpace . reverse . dropWhile Char.isSpace . reverse

createLocationWeatherLookup :: WebHandler (M.Map Int LocationWeatherResponse)
createLocationWeatherLookup = do
  locationWeathers <- runApplication DB.getLocationWeathers
  return $
    M.fromList $
      [ ( locationID,
          LocationWeatherResponse icon (capitalized description) (kelvinToCelsius temperature) (metersPerSecondToMilesPerHour windSpeed) windDirection (directionToCardinal windDirection)
        )
        | (LocationWeather locationID description icon temperature windSpeed windDirection _ _) <-
            locationWeathers
      ]
  where
    kelvinToCelsius :: Scientific -> Int
    kelvinToCelsius kelvin = round . toRealFloat $ kelvin - 273.15

    metersPerSecondToMilesPerHour :: Scientific -> Int
    metersPerSecondToMilesPerHour mps = round . toRealFloat $ mps * 2.236936284

    directionToCardinal :: Scientific -> String
    directionToCardinal degrees =
      let index = floor ((toRealFloat degrees + 11.25) / 22.5)
          cardinals = ["N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"]
       in cardinals !! (index `mod` 16)

    capitalized :: String -> String
    capitalized (x : xs) = Char.toUpper x : map Char.toLower xs
    capitalized [] = []

getLatitude :: Geometry -> Scientific
getLatitude (GeoPoint _ (Point (Position latitude _ _ _))) = fromFloatDigits latitude
getLatitude _ = error "Expected point"

getLongitude :: Geometry -> Scientific
getLongitude (GeoPoint _ (Point (Position _ longitude _ _))) = fromFloatDigits longitude
getLongitude _ = error "Expected point"

vesselTimeFilter :: UTCTime -> UTCTime -> Bool
vesselTimeFilter currentTime lastReceived =
  let diff = diffUTCTime currentTime lastReceived
   in diff < 1800

convertLocalTimeToUTC :: LocalTime -> UTCTime
convertLocalTimeToUTC = localTimeToUTC (read "UTC")
