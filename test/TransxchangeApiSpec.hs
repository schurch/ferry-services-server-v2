{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module TransxchangeApiSpec where

import Control.Monad (void)
import Control.Monad.Reader (ReaderT (runReaderT))
import App.Database (createConnectionPool)
import App.Logger
  ( Logger,
    Output (StdOut),
    create,
  )
import Data.Aeson (decode)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Pool
  ( Pool,
    withResource,
  )
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple
  ( Connection,
    In (In),
    Only (Only),
    execute,
    execute_,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Network.Wai (Application, queryString, rawPathInfo, rawQueryString, requestMethod)
import Network.Wai.Test (SResponse (simpleBody), defaultRequest, request, runSession, setPath)
import Network.Wai.Middleware.RequestLogger (mkRequestLogger)
import System.Environment (lookupEnv, setEnv)
import Test.Hspec
  ( Spec,
    Expectation,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
  )
import TransxchangeV2.Ingest (ingestDirectoryV2)
import TransxchangeV2.Repository (clearTx2Tables)
import App.Env
  ( Env (Env, connectionPool, logger),
  )
import Types
  ( DepartureResponse (..),
    LocationResponse (..),
    ServiceResponse (..),
    ServiceStatus (Unknown),
  )
import Web.Scotty.Trans (defaultOptions, scottyAppT)
import WebServer (loggerSettings, webApp)

spec :: Spec
spec = do
  describe "TransXChange API Tests" $ do
    it "returns expected departures for a basic real mapped service" $ do
      context <- setupTx2ApiTests
      resetTx2ApiState context basicRealScenario
      seedScenarioService context basicRealScenario
      ingestScenarioFixture context basicRealScenario
      response <- getServiceResponseForDate context basicRealScenario
      assertBasicRealPentlandDepartures response

    it "suppresses departures on a real non-operation date" $ do
      context <- setupTx2ApiTests
      resetTx2ApiState context nonOperationRealScenario
      seedScenarioService context nonOperationRealScenario
      ingestScenarioFixture context nonOperationRealScenario
      excludedResponse <- getServiceResponseForDate context nonOperationRealScenario
      assertScheduledDepartureCounts [30, 30] excludedResponse

    it "returns the expected departures for a real bank holiday scenario" $ do
      context <- setupTx2ApiTests
      resetTx2ApiState context bankHolidayRealScenario
      seedScenarioService context bankHolidayRealScenario
      ingestScenarioFixture context bankHolidayRealScenario
      holidayResponse <- getServiceResponseForDate context bankHolidayRealScenario
      assertScheduledDepartureCounts [28, 28] holidayResponse

    it "selects departures from the correct real file when multiple CalMac files exist for one service" $ do
      context <- setupTx2ApiTests
      resetTx2ApiState context multiFileSameServiceRealScenario
      seedScenarioService context multiFileSameServiceRealScenario
      ingestScenarioFixture context multiFileSameServiceRealScenario
      saturdayResponse <-
        getServiceResponseForDay
          context
          (scenarioServiceId $ tx2ScenarioSeed multiFileSameServiceRealScenario)
          (fromGregorian 2026 3 14)
      sundayResponse <-
        getServiceResponseForDay
          context
          (scenarioServiceId $ tx2ScenarioSeed multiFileSameServiceRealScenario)
          (fromGregorian 2026 3 15)
      assertCalmacCm21BoundaryDepartures saturdayResponse sundayResponse

data TestContext = TestContext
  { testContextApp :: Application,
    testContextConnectionPool :: Pool Connection,
    testContextLogger :: Logger
  }

data Tx2ApiScenario = Tx2ApiScenario
  { tx2ScenarioName :: String,
    tx2ScenarioFixtureDir :: FilePath,
    tx2ScenarioQueryDate :: Day,
    tx2ScenarioSeed :: ScenarioSeed,
    tx2ScenarioExpectationSummary :: [String]
  }

data ScenarioSeed = ScenarioSeed
  { scenarioServiceId :: Int,
    scenarioArea :: String,
    scenarioRoute :: String,
    scenarioOrganisationId :: Int,
    scenarioServiceCode :: String,
    scenarioLocations :: [ScenarioLocation]
  }

data ScenarioLocation = ScenarioLocation
  { scenarioLocationId :: Int,
    scenarioLocationName :: String,
    scenarioStopPointId :: String,
    scenarioLatitude :: Double,
    scenarioLongitude :: Double
  }

setupTx2ApiTests :: IO TestContext
setupTx2ApiTests = do
  logger <- create StdOut
  requestLogger <- mkRequestLogger $ loggerSettings logger
  connectionString <- getDbConnectionString
  connectionPool <- createConnectionPool connectionString
  app <- scottyAppT defaultOptions (`runReaderT` Env logger connectionPool) (webApp requestLogger)
  pure $
    TestContext
      { testContextApp = app,
        testContextConnectionPool = connectionPool,
        testContextLogger = logger
      }

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

resetTx2ApiState :: TestContext -> Tx2ApiScenario -> IO ()
resetTx2ApiState context scenario =
  withResource (testContextConnectionPool context) $ \connection -> do
    clearTx2Tables connection
    void $
      execute
        connection
        "DELETE FROM tx2_service_mappings WHERE service_id BETWEEN 9100 AND 9799"
        ()
    void $
      execute
        connection
        "DELETE FROM service_locations WHERE service_id BETWEEN 9100 AND 9799"
        ()
    void $
      execute
        connection
        "DELETE FROM service_locations WHERE location_id IN (SELECT location_id FROM locations WHERE stop_point_id IN ?)"
        (Only $ In $ scenarioStopPointId <$> scenarioLocations (tx2ScenarioSeed scenario))
    void $
      execute
        connection
        "DELETE FROM services WHERE service_id BETWEEN 9100 AND 9799"
        ()
    void $
      execute
        connection
        "DELETE FROM locations WHERE location_id BETWEEN 9100 AND 9799"
        ()
    void $
      execute
        connection
        "DELETE FROM locations WHERE stop_point_id IN ?"
        (Only $ In $ scenarioStopPointId <$> scenarioLocations (tx2ScenarioSeed scenario))

seedScenarioService :: TestContext -> Tx2ApiScenario -> IO ()
seedScenarioService context scenario =
  withResource (testContextConnectionPool context) $ \connection -> do
    let seed = tx2ScenarioSeed scenario
    void $
      execute_
        connection
        [sql|
          INSERT INTO organisations (organisation_id, name)
          VALUES (999, 'TX2 API Test Organisation')
          ON CONFLICT (organisation_id) DO NOTHING
        |]
    void $
      execute
        connection
        [sql|
          INSERT INTO services (service_id, area, route, organisation_id, status, updated)
          VALUES (?, ?, ?, ?, ?, now())
        |]
        (scenarioServiceId seed, scenarioArea seed, scenarioRoute seed, scenarioOrganisationId seed, Unknown)
    mapM_ (insertScenarioLocation connection) (scenarioLocations seed)
    mapM_ (insertScenarioServiceLocation connection (scenarioServiceId seed)) (scenarioLocations seed)
    void $
      execute
        connection
        [sql|
          INSERT INTO tx2_service_mappings (service_id, service_code)
          VALUES (?, ?)
        |]
        (scenarioServiceId seed, scenarioServiceCode seed)

ingestScenarioFixture :: TestContext -> Tx2ApiScenario -> IO ()
ingestScenarioFixture context scenario =
  void $ runReaderT (ingestDirectoryV2 $ tx2ScenarioFixtureDir scenario) (toEnv context)

getServiceResponseForDate :: TestContext -> Tx2ApiScenario -> IO ServiceResponse
getServiceResponseForDate context scenario = do
  let serviceId = scenarioServiceId $ tx2ScenarioSeed scenario
  getServiceResponseForDay context serviceId (tx2ScenarioQueryDate scenario)

getServiceResponseForDay :: TestContext -> Int -> Day -> IO ServiceResponse
getServiceResponseForDay context serviceId queryDate = do
  let path = "/api/services/" <> show serviceId
      rawQuery = "?departuresDate=" <> show queryDate
      queryValue = B8.pack (show queryDate)
  response <-
    runSession
      ( request
          (setPath defaultRequest (B8.pack path))
            { rawPathInfo = B8.pack path,
              rawQueryString = B8.pack rawQuery,
              queryString = [("departuresDate", Just queryValue)],
              requestMethod = "GET"
            }
      )
      (testContextApp context)
  let body = simpleBody response
  case decode body of
    Just (Just serviceResponse) -> pure serviceResponse
    Just Nothing -> error "Expected API service response but got null"
    Nothing -> error ("Failed to decode service response JSON: " <> show body)

assertBasicRealPentlandDepartures :: ServiceResponse -> Expectation
assertBasicRealPentlandDepartures response = do
  serviceResponseServiceID response `shouldBe` 9100
  serviceResponseScheduledDeparturesAvailable response `shouldBe` Just True
  let gillsBay = findLocationResponse 9101 response
      stMargaretsHope = findLocationResponse 9102 response
      gillsDepartures = fromMaybe [] (locationResponseScheduledDepartures gillsBay)
      stMargaretsDepartures = fromMaybe [] (locationResponseScheduledDepartures stMargaretsHope)
  locationResponseName gillsBay `shouldBe` "Gills Bay"
  locationResponseName stMargaretsHope `shouldBe` "St Margaret's Hope"
  map departureResponseDeparture gillsDepartures `shouldBe` map readUtc ["2026-03-16 09:30:00 UTC", "2026-03-16 13:30:00 UTC", "2026-03-16 18:45:00 UTC"]
  map departureResponseArrival gillsDepartures `shouldBe` map readUtc ["2026-03-16 10:40:00 UTC", "2026-03-16 14:40:00 UTC", "2026-03-16 19:55:00 UTC"]
  map (locationResponseID . departureResponseDestination) gillsDepartures `shouldBe` [9102, 9102, 9102]
  map departureResponseDeparture stMargaretsDepartures `shouldBe` map readUtc ["2026-03-16 07:45:00 UTC", "2026-03-16 11:30:00 UTC", "2026-03-16 17:00:00 UTC"]
  map departureResponseArrival stMargaretsDepartures `shouldBe` map readUtc ["2026-03-16 08:55:00 UTC", "2026-03-16 12:40:00 UTC", "2026-03-16 18:10:00 UTC"]
  map (locationResponseID . departureResponseDestination) stMargaretsDepartures `shouldBe` [9101, 9101, 9101]
  serviceResponseUpdated response `shouldSatisfy` const True

assertCalmacCm21BoundaryDepartures :: ServiceResponse -> ServiceResponse -> Expectation
assertCalmacCm21BoundaryDepartures saturdayResponse sundayResponse = do
  serviceResponseServiceID saturdayResponse `shouldBe` 9700
  serviceResponseServiceID sundayResponse `shouldBe` 9700
  serviceResponseScheduledDeparturesAvailable saturdayResponse `shouldBe` Just True
  serviceResponseScheduledDeparturesAvailable sundayResponse `shouldBe` Just True
  let saturdayBarra = findLocationResponse 9701 saturdayResponse
      saturdayEriskay = findLocationResponse 9702 saturdayResponse
      sundayBarra = findLocationResponse 9701 sundayResponse
      sundayEriskay = findLocationResponse 9702 sundayResponse
      saturdayBarraDepartures = fromMaybe [] (locationResponseScheduledDepartures saturdayBarra)
      saturdayEriskayDepartures = fromMaybe [] (locationResponseScheduledDepartures saturdayEriskay)
      sundayBarraDepartures = fromMaybe [] (locationResponseScheduledDepartures sundayBarra)
      sundayEriskayDepartures = fromMaybe [] (locationResponseScheduledDepartures sundayEriskay)
  locationResponseName saturdayBarra `shouldBe` "Aird Mhor Barra"
  locationResponseName saturdayEriskay `shouldBe` "Eriskay"
  map departureResponseDeparture saturdayBarraDepartures
    `shouldBe` map readUtc ["2026-03-14 07:15:00 UTC", "2026-03-14 09:15:00 UTC", "2026-03-14 11:10:00 UTC", "2026-03-14 15:40:00 UTC", "2026-03-14 17:20:00 UTC"]
  map departureResponseDeparture saturdayEriskayDepartures
    `shouldBe` map readUtc ["2026-03-14 08:10:00 UTC", "2026-03-14 10:10:00 UTC", "2026-03-14 13:00:00 UTC", "2026-03-14 16:30:00 UTC", "2026-03-14 18:15:00 UTC"]
  map departureResponseDeparture sundayBarraDepartures
    `shouldBe` map readUtc ["2026-03-15 08:45:00 UTC", "2026-03-15 16:30:00 UTC"]
  map departureResponseDeparture sundayEriskayDepartures
    `shouldBe` map readUtc ["2026-03-15 09:30:00 UTC", "2026-03-15 17:20:00 UTC"]
  map departureResponseNotes sundayBarraDepartures
    `shouldBe`
      [ Just "Passengers should check-in 10 mins before ferry departure time shown | Book by 1400 on day before travel.",
        Just "Passengers should check-in 10 mins before ferry departure time shown"
      ]
  map departureResponseNotes sundayEriskayDepartures
    `shouldBe`
      [ Just "Passengers should check-in 10 mins before ferry departure time shown | Book by 1400 on day before travel.",
        Just "Passengers should check-in 10 mins before ferry departure time shown"
      ]

assertNoDeparturesAtLocation :: Int -> ServiceResponse -> Expectation
assertNoDeparturesAtLocation locationId response = do
  let location = findLocationResponse locationId response
  fromMaybe [] (locationResponseScheduledDepartures location) `shouldSatisfy` null

assertHasDeparturesAtLocation :: Int -> ServiceResponse -> Expectation
assertHasDeparturesAtLocation locationId response = do
  let location = findLocationResponse locationId response
  fromMaybe [] (locationResponseScheduledDepartures location) `shouldSatisfy` (not . null)

assertScheduledDepartureCounts :: [Int] -> ServiceResponse -> Expectation
assertScheduledDepartureCounts expectedCounts response = do
  let counts = scheduledDepartureCounts response
  counts `shouldBe` expectedCounts
  serviceResponseScheduledDeparturesAvailable response `shouldBe` Just True

scheduledDepartureCounts :: ServiceResponse -> [Int]
scheduledDepartureCounts response =
  map (length . fromMaybe [] . locationResponseScheduledDepartures) (serviceResponseLocations response)

insertScenarioLocation :: Connection -> ScenarioLocation -> IO ()
insertScenarioLocation connection ScenarioLocation {..} =
  void $
    execute
      connection
      [sql|
        INSERT INTO locations (location_id, name, coordinate, stop_point_id)
        VALUES (?, ?, ST_SetSRID(ST_Point(?, ?), 4326), ?)
      |]
      (scenarioLocationId, scenarioLocationName, scenarioLatitude, scenarioLongitude, scenarioStopPointId)

insertScenarioServiceLocation :: Connection -> Int -> ScenarioLocation -> IO ()
insertScenarioServiceLocation connection serviceId ScenarioLocation {..} =
  void $
    execute
      connection
      [sql|
        INSERT INTO service_locations (service_id, location_id)
        VALUES (?, ?)
      |]
      (serviceId, scenarioLocationId)

findLocationResponse :: Int -> ServiceResponse -> LocationResponse
findLocationResponse locationId response =
  fromMaybe
    (error ("Missing location in service response: " <> show locationId))
    (find ((== locationId) . locationResponseID) (serviceResponseLocations response))

readUtc :: String -> UTCTime
readUtc = read

toEnv :: TestContext -> Env
toEnv context =
  Env
    { logger = testContextLogger context,
      connectionPool = testContextConnectionPool context
    }

basicRealScenario :: Tx2ApiScenario
basicRealScenario =
  Tx2ApiScenario
    { tx2ScenarioName = "01-basic-real",
      tx2ScenarioFixtureDir = "test/fixtures/transxchange-api/01-basic-real",
      tx2ScenarioQueryDate = fromGregorian 2026 3 16,
      tx2ScenarioSeed =
        ScenarioSeed
          { scenarioServiceId = 9100,
            scenarioArea = "PENTLAND FIRTH",
            scenarioRoute = "Gills Bay - St Margaret's Hope",
            scenarioOrganisationId = 999,
            scenarioServiceCode = "PENT_PF1",
            scenarioLocations =
              [ ScenarioLocation 9101 "Gills Bay" "9300GIL" 58.63917534851306 (-3.1614340648459605),
                ScenarioLocation 9102 "St Margaret's Hope" "9300SMH" 58.832021233320255 (-2.9622400477352535)
              ]
          },
      tx2ScenarioExpectationSummary =
        [ "Uses the real Pentland PF1 file copied into this fixture directory",
          "Queries a Monday within the operating period",
          "Asserts the exact API departures and arrivals for both terminals"
        ]
    }

nonOperationRealScenario :: Tx2ApiScenario
nonOperationRealScenario =
  Tx2ApiScenario
    { tx2ScenarioName = "03-non-operation-real",
      tx2ScenarioFixtureDir = "test/fixtures/transxchange-api/03-non-operation-real",
      tx2ScenarioQueryDate = fromGregorian 2026 5 22,
      tx2ScenarioSeed =
        ScenarioSeed
          { scenarioServiceId = 9300,
            scenarioArea = "ARGYLL",
            scenarioRoute = "North Cuan Seil - South Cuan Luing",
            scenarioOrganisationId = 999,
            scenarioServiceCode = "ABCF_LUI",
            scenarioLocations =
              [ ScenarioLocation 9301 "North Cuan Seil" "9300CUN" 56.2473 (-5.6288),
                ScenarioLocation 9302 "South Cuan Luing" "9300LUI" 56.2456 (-5.6296)
              ]
          },
      tx2ScenarioExpectationSummary =
        [ "Uses the real ABCF_LUI ferry file with explicit SpecialDaysOperation DaysOfNonOperation ranges",
          "Asserts the exact per-terminal departure counts returned by the API on 2026-05-22",
          "Keeps the fixture ferry-only so the API departure path is exercised end to end"
        ]
    }

bankHolidayRealScenario :: Tx2ApiScenario
bankHolidayRealScenario =
  Tx2ApiScenario
    { tx2ScenarioName = "04-bank-holiday-real",
      tx2ScenarioFixtureDir = "test/fixtures/transxchange-api/04-bank-holiday-real",
      tx2ScenarioQueryDate = fromGregorian 2026 5 4,
      tx2ScenarioSeed =
        ScenarioSeed
          { scenarioServiceId = 9400,
            scenarioArea = "ARGYLL",
            scenarioRoute = "North Cuan Seil - South Cuan Luing",
            scenarioOrganisationId = 999,
            scenarioServiceCode = "ABCF_LUI",
            scenarioLocations =
              [ ScenarioLocation 9401 "North Cuan Seil" "9300CUN" 56.2473 (-5.6288),
                ScenarioLocation 9402 "South Cuan Luing" "9300LUI" 56.2456 (-5.6296)
              ]
          },
      tx2ScenarioExpectationSummary =
        [ "Uses the real ABCF_LUI ferry file with explicit MayDay bank-holiday non-operation rules",
          "Asserts the exact per-terminal departure counts returned by the API on 2026-05-04",
          "Keeps the fixture ferry-only so the API departure path is exercised end to end"
        ]
    }

multiFileSameServiceRealScenario :: Tx2ApiScenario
multiFileSameServiceRealScenario =
  Tx2ApiScenario
    { tx2ScenarioName = "07-multi-file-same-service-real",
      tx2ScenarioFixtureDir = "test/fixtures/transxchange-api/07-multi-file-same-service-real",
      tx2ScenarioQueryDate = fromGregorian 2026 3 14,
      tx2ScenarioSeed =
        ScenarioSeed
          { scenarioServiceId = 9700,
            scenarioArea = "OUTER HEBRIDES",
            scenarioRoute = "Barra - Eriskay",
            scenarioOrganisationId = 999,
            scenarioServiceCode = "CALM_CM21",
            scenarioLocations =
              [ ScenarioLocation 9701 "Aird Mhor Barra" "9300AHB" 57.0155 (-7.4429),
                ScenarioLocation 9702 "Eriskay" "9300ERI" 57.0822 (-7.2958)
              ]
          },
      tx2ScenarioExpectationSummary =
        [ "Uses two real consecutive CalMac CM21 files for the same service code in one fixture directory",
          "Queries adjacent dates on either side of the file-boundary transition",
          "Asserts the API returns the Saturday timetable from the earlier file and the Sunday timetable from the later file"
        ]
    }
