{-# LANGUAGE OverloadedStrings #-}

module IntegrationSpec where

import Control.Monad.Reader
  ( ReaderT (runReaderT),
    asks,
  )
import App.Database (createConnectionPool)
import App.Env (Env (Env))
import qualified App.Env
import App.Logger
  ( Logger,
    Output (StdOut),
    create,
  )
import Data.Aeson (FromJSON, decode, encode)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.List (find, isPrefixOf)
import Data.Pool
  ( Pool,
    withResource,
  )
import qualified Database as DB
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types.Header (Header, hContentType)
import Network.HTTP.Types.Method (methodGet, methodPost)
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (mkRequestLogger)
import Scraper
import System.Environment (lookupEnv, setEnv)
import System.Directory (createDirectoryIfMissing)
import Test.Hspec
  ( Spec,
    beforeAll,
    beforeAll_,
    before_,
    describe,
    hspec,
    it,
  )
import Test.Hspec.Wai
  ( Body,
    MatchBody (MatchBody),
    ResponseMatcher (ResponseMatcher),
    delete,
    get,
    request,
    shouldRespondWith,
    with,
  )
import Types
import Types.Api
import Types.Weather
import VesselFetcher
import WebServer

spec :: Spec
spec = beforeAll_ setupIntegrationTests $ with app $ do
  describe "GET /openapi.json" $ do
    it "responds with 200" $ do
      get "/openapi.json" `shouldRespondWith` 200

  describe "GET /swagger" $ do
    it "responds with 200" $ do
      get "/swagger" `shouldRespondWith` 200

  describe "GET /api/services" $ do
    it "responds with 200" $ do
      get "/api/services" `shouldRespondWith` serviceList

  describe "GET /api/services/5" $ do
    it "responds with 200" $ do
      get "/api/services/5" `shouldRespondWith` arranService

  describe "POST /api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31" $ do
    it "responds with 200" $ do
      let requestBody = CreateInstallationRequest "20f2d2e336a0c62c8c5f3dc1bb3d3f36e830ce14230bc8fa547fd16fb8f917b5" IOS
      jsonPost "/api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31" (encode requestBody) `shouldRespondWith` emptyServiceList

  describe "POST /api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31/services" $ do
    it "responds with 200" $ do
      let requestBody = AddServiceRequest 5
      jsonPost "/api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31/services" (encode requestBody) `shouldRespondWith` registeredService

  describe "GET /api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31/services" $ do
    it "responds with 200" $ do
      get "/api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31/services" `shouldRespondWith` registeredService

  describe "DELETE /api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31/services/5" $ do
    it "responds with 200" $ do
      delete "/api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31/services/5" `shouldRespondWith` emptyServiceList

  describe "GET /api/vessels" $ do
    it "responds with 200" $ do
      get "/api/vessels" `shouldRespondWith` vessels

  describe "GET /api/timetable-documents" $ do
    it "serves the timetable document catalogue with cache headers" $ do
      get "/api/timetable-documents?serviceID=-1" `shouldRespondWith` timetableDocuments

    it "returns 304 when the client already has the current ETag" $ do
      request methodGet "/api/timetable-documents?serviceID=-1" [("If-None-Match", "\"sha256-4f53cda18c2baa0c0354bb5f9a3ecbe5ed12ab4d8e11ba873c2f11161202b945\"")] ""
        `shouldRespondWith` 304

    it "returns 304 when Cloudflare has weakened the current ETag" $ do
      request methodGet "/api/timetable-documents?serviceID=-1" [("If-None-Match", "W/\"sha256-4f53cda18c2baa0c0354bb5f9a3ecbe5ed12ab4d8e11ba873c2f11161202b945\"")] ""
        `shouldRespondWith` 304

  describe "GET /api/offline/snapshot.json" $ before_ setupOfflineSnapshotFixture $ do
    it "serves the generated offline snapshot with cache headers" $ do
      get "/api/offline/snapshot.json" `shouldRespondWith` offlineSnapshot

    it "returns 304 when the client already has the current ETag" $ do
      request methodGet "/api/offline/snapshot.json" [("If-None-Match", "\"sha256-test\"")] ""
        `shouldRespondWith` 304

    it "returns 304 when Cloudflare has weakened the current ETag" $ do
      request methodGet "/api/offline/snapshot.json" [("If-None-Match", "W/\"sha256-test\"")] ""
        `shouldRespondWith` 304

-- Response checks
vessels :: ResponseMatcher
vessels = jsonReponseMatcher checkVesselsResponse
  where
    checkVesselsResponse :: [VesselResponse] -> Maybe String
    checkVesselsResponse [VesselResponse caledonianIslesMMSI "Caledonian Isles" _ _ _ _ _] = Nothing
    checkVesselsResponse response = return $ "Unexpected response: " <> show response

timetableDocuments :: ResponseMatcher
timetableDocuments =
  ResponseMatcher
    200
    []
    (MatchBody bodyMatcher)
  where
    bodyMatcher headers body
      | lookup "Cache-Control" headers /= Just "private, no-cache, no-transform" =
          Just $ "Unexpected Cache-Control: " <> show (lookup "Cache-Control" headers)
      | lookup "ETag" headers /= Just "\"sha256-4f53cda18c2baa0c0354bb5f9a3ecbe5ed12ab4d8e11ba873c2f11161202b945\"" =
          Just $ "Unexpected ETag: " <> show (lookup "ETag" headers)
      | body /= "[]" =
          Just $ "Unexpected body: " <> B8.unpack (BL.toStrict body)
      | otherwise = Nothing

emptyServiceList :: ResponseMatcher
emptyServiceList = jsonReponseMatcher checkEmptyServicesResponse
  where
    checkEmptyServicesResponse :: [ServiceResponse] -> Maybe String
    checkEmptyServicesResponse [] = Nothing
    checkEmptyServicesResponse response = return $ "Unexpected response: " <> show response

serviceList :: ResponseMatcher
serviceList = jsonReponseMatcher checkServiceListResponse
  where
    checkServiceListResponse :: [ServiceResponse] -> Maybe String
    checkServiceListResponse _ = Nothing

registeredService :: ResponseMatcher
registeredService = jsonReponseMatcher checkRegisteredServiceResponse
  where
    checkRegisteredServiceResponse :: [ServiceResponse] -> Maybe String
    checkRegisteredServiceResponse [service] = checkArranServiceResponse service
    checkRegisteredServiceResponse response = return $ "Unexpected response: " <> show response

arranService :: ResponseMatcher
arranService = jsonReponseMatcher checkArranServiceResponse

offlineSnapshot :: ResponseMatcher
offlineSnapshot =
  ResponseMatcher
    200
    []
    (MatchBody bodyMatcher)
  where
    bodyMatcher headers body
      | lookup "Cache-Control" headers /= Just "public, max-age=900, stale-while-revalidate=86400" =
          Just $ "Unexpected Cache-Control: " <> show (lookup "Cache-Control" headers)
      | lookup "ETag" headers /= Just "\"sha256-test\"" =
          Just $ "Unexpected ETag: " <> show (lookup "ETag" headers)
      | body /= offlineSnapshotBody =
          Just $ "Unexpected body: " <> B8.unpack (BL.toStrict body)
      | otherwise = Nothing

offlineSnapshotBody :: BL.ByteString
offlineSnapshotBody =
  "{\"schema_version\":1,\"data_version\":\"sha256-test\",\"generated_at\":\"2026-05-02T00:00:00Z\",\"valid_from\":\"2026-05-02\",\"valid_to\":\"2026-06-30\",\"services\":[],\"locations\":[],\"organisations\":[],\"timetable_documents\":[],\"departures\":[]}"

offlineSnapshotMetadataBody :: BL.ByteString
offlineSnapshotMetadataBody =
  "{\"data_version\":\"sha256-test\",\"etag\":\"\\\"sha256-test\\\"\",\"generated_at\":\"2026-05-02T00:00:00Z\",\"valid_from\":\"2026-05-02\",\"valid_to\":\"2026-06-30\"}"

checkArranServiceResponse :: ServiceResponse -> Maybe String
checkArranServiceResponse response
  | serviceResponseServiceID response == 5
      && any ((==) 4 . locationResponseID) (serviceResponseLocations response) =
      Nothing
  | otherwise = return $ "Unexpected response: " <> show response

jsonReponseMatcher :: FromJSON a => (a -> Maybe String) -> ResponseMatcher
jsonReponseMatcher jsonChecker = do
  ResponseMatcher 200 [] (MatchBody bodyMatcher)
  where
    bodyMatcher :: [Header] -> Body -> Maybe String
    bodyMatcher _ body = case decode body of
      Just decodedBody -> jsonChecker decodedBody
      Nothing -> return "Couldn't decode response"

jsonPost path =
  request methodPost path [(hContentType, "application/json")]

-- Setup
setupIntegrationTests :: IO ()
setupIntegrationTests = do
  setEnv "FERRY_SERVICES_TEST_AWS_ENDPOINT_ARN" "arn:aws:sns:ap-southeast-2:000000000000:endpoint/APNS_SANDBOX/test/test"
  logger <- create StdOut
  connectionString <- getDbConnectionString
  connectionPool <- createConnectionPool connectionString
  let env = Env logger connectionPool
  runReaderT (runScraper >> seedBrodickWeather >> fetchCaledonianIslesVessel) env
  where
    runScraper :: App.Env.Application ()
    runScraper = do
      fetchNorthLinkServicesAndNotify
      fetchCalMacStatusesAndNotify

    seedBrodickWeather :: App.Env.Application ()
    seedBrodickWeather =
      DB.insertLocationWeather
        4
        ( WeatherFetcherResult
            [WeatherFetcherResultWeather "10n" "moderate rain"]
            (WeatherFetcherResultMain 278.15)
            (WeatherFetcherResultWind 8.04672 196.4)
        )

    fetchCaledonianIslesVessel :: App.Env.Application ()
    fetchCaledonianIslesVessel = fetchVessels [(calMacOrganisationID, [caledonianIslesMMSI])]

calMacOrganisationID :: Int
calMacOrganisationID = 1

caledonianIslesMMSI :: Int
caledonianIslesMMSI = 232001580

app :: IO Network.Wai.Application
app = do
  logger <- create StdOut
  requestLogger <- mkRequestLogger $ loggerSettings logger
  connectionString <- getDbConnectionString
  connectionPool <- createConnectionPool connectionString
  return $ webApp (Env logger connectionPool) requestLogger

getDbConnectionString :: IO String
getDbConnectionString = do
  current <- lookupEnv "DB_CONNECTION"
  case current of
    Just value | not (null value) -> return value
    _ -> do
      envfile <- readFile "envfile-test.local"
      let prefix = "DB_CONNECTION=" :: String
      let match = find (prefix `isPrefixOf`) (lines envfile)
      case match of
        Just line -> do
          let value = drop (length prefix) line
          setEnv "DB_CONNECTION" value
          return value
        Nothing -> error "DB_CONNECTION missing from env and envfile-test.local"

setupOfflineSnapshotFixture :: IO ()
setupOfflineSnapshotFixture = do
  createDirectoryIfMissing True "offline"
  BL.writeFile "offline/snapshot.json" offlineSnapshotBody
  BL.writeFile "offline/snapshot.meta.json" offlineSnapshotMetadataBody
