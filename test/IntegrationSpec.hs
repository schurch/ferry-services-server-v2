{-# LANGUAGE OverloadedStrings #-}

module IntegrationSpec where

import Control.Monad.Reader
  ( ReaderT (runReaderT),
    asks,
  )
import Data.Aeson (FromJSON, decode, encode)
import Data.List (find)
import Data.Pool
  ( Pool,
    createPool,
    withResource,
  )
import Data.String (fromString)
import qualified Database as DB
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Network.HTTP.Types.Header (Header)
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (mkRequestLogger)
import Scraper
import System.Environment (getEnv, setEnv)
import System.Logger
  ( Logger,
    Output (StdOut),
    create,
  )
import Test.Hspec
  ( Spec,
    beforeAll,
    beforeAll_,
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
    post,
    shouldRespondWith,
    with,
  )
import Types
import VesselFetcher
import WeatherFetcher
import Web.Scotty.Trans (scottyAppT)
import WebServer

spec :: Spec
spec = beforeAll_ setupIntegrationTests $ with app $ do
  describe "GET /api/services" $ do
    it "responds with 200" $ do
      get "/api/services" `shouldRespondWith` serviceList

  describe "GET /api/services/5" $ do
    it "responds with 200" $ do
      get "/api/services/5" `shouldRespondWith` arranService

  describe "POST /api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31" $ do
    it "responds with 200" $ do
      let requestBody = CreateInstallationRequest "20f2d2e336a0c62c8c5f3dc1bb3d3f36e830ce14230bc8fa547fd16fb8f917b5" IOS
      post "/api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31" (encode requestBody) `shouldRespondWith` emptyServiceList

  describe "POST /api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31/services" $ do
    it "responds with 200" $ do
      let requestBody = AddServiceRequest 5
      post "/api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31/services" (encode requestBody) `shouldRespondWith` registeredService

  describe "GET /api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31/services" $ do
    it "responds with 200" $ do
      get "/api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31/services" `shouldRespondWith` registeredService

  describe "DELETE /api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31/services/5" $ do
    it "responds with 200" $ do
      delete "/api/installations/DAA2D656-4824-4137-A4CD-E44FBB135A31/services/5" `shouldRespondWith` emptyServiceList

  describe "GET /api/vessels" $ do
    it "responds with 200" $ do
      get "/api/vessels" `shouldRespondWith` vessels

-- Response checks
vessels :: ResponseMatcher
vessels = jsonReponseMatcher checkVesselsResponse
  where
    checkVesselsResponse :: [VesselResponse] -> Maybe String
    checkVesselsResponse [VesselResponse caledonianIslesMMSI "Caledonian Isles" _ _ _ _ _] = Nothing
    checkVesselsResponse response = return $ "Unexpected response: " <> show response

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

checkArranServiceResponse :: ServiceResponse -> Maybe String
checkArranServiceResponse (ServiceResponse 5 _ "ARRAN" "Ardrossan (ARD) - Brodick (BRO)" _ _ _ _ _ _ _ _) = Nothing
checkArranServiceResponse response = return $ "Unexpected response: " <> show response

jsonReponseMatcher :: FromJSON a => (a -> Maybe String) -> ResponseMatcher
jsonReponseMatcher jsonChecker = do
  ResponseMatcher 200 [] (MatchBody bodyMatcher)
  where
    bodyMatcher :: [Header] -> Body -> Maybe String
    bodyMatcher _ body = case decode body of
      Just decodedBody -> jsonChecker decodedBody
      Nothing -> return "Couldn't decode response"

-- Setup
setupIntegrationTests :: IO ()
setupIntegrationTests = do
  logger <- create StdOut
  connectionString <- getEnv "DB_CONNECTION"
  connectionPool <-
    createPool
      (connectPostgreSQL $ fromString connectionString)
      Database.PostgreSQL.Simple.close
      2 -- stripes
      60 -- unused connections are kept open for a minute
      10 -- max. 10 connections open per stripe
  let env = Env logger connectionPool
  runReaderT (runScraper >> fetchBrodickWeather >> fetchCaledonianIslesVessel) env
  where
    runScraper :: Types.Application ()
    runScraper = do
      fetchNorthLinkServicesAndNotify
      fetchCalMacStatusesAndNotify

    fetchBrodickWeather :: Types.Application ()
    fetchBrodickWeather = do
      brodick <- filter ((==) 4 . locationLocationID) <$> DB.getLocations
      fetchWeatherForLocations brodick

    fetchCaledonianIslesVessel :: Types.Application ()
    fetchCaledonianIslesVessel = fetchVessels [(calMacOrganisationID, [caledonianIslesMMSI])]

calMacOrganisationID :: Int
calMacOrganisationID = 1

caledonianIslesMMSI :: Int
caledonianIslesMMSI = 232001580

app :: IO Network.Wai.Application
app = do
  logger <- create StdOut
  requestLogger <- mkRequestLogger $ loggerSettings logger
  connectionString <- getEnv "DB_CONNECTION"
  connectionPool <-
    createPool
      (connectPostgreSQL $ fromString connectionString)
      Database.PostgreSQL.Simple.close
      2 -- stripes
      60 -- unused connections are kept open for a minute
      10 -- max. 10 connections open per stripe
  scottyAppT (`runReaderT` Env logger connectionPool) (webApp requestLogger)
