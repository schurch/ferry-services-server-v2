{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Network.Socket                 ( defaultHints
                                                , getAddrInfo
                                                , withSocketsDo
                                                , connect
                                                , socket
                                                , close
                                                , AddrInfo
                                                  ( addrAddress
                                                  , addrFamily
                                                  , addrSocketType
                                                  , addrProtocol
                                                  )
                                                , HostName
                                                , ServiceName
                                                , Socket
                                                , SocketType(Stream)
                                                )
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )
import           Data.Char                      ( isSpace )
import           Data.List                      ( dropWhileEnd
                                                , intercalate
                                                )
import           Control.Concurrent             ( newQSem
                                                , signalQSem
                                                , waitQSem
                                                , forkIO
                                                , threadDelay
                                                )
import           System.Directory               ( doesFileExist
                                                , removeFile
                                                , removeDirectoryRecursive
                                                )
import           System.Environment             ( getEnv )
import           Control.Monad                  ( forever
                                                , when
                                                , void
                                                , forM
                                                )
import           GHC.Generics                   ( Generic )
import           Data.Csv                       ( (.:)
                                                , decodeByName
                                                , FromNamedRecord(..)
                                                )
import           Data.Map                       ( fromListWith
                                                , toList
                                                )
import           Codec.Archive.Zip              ( unpackInto
                                                , withArchive
                                                )
import           System.Log.Raven               ( initRaven
                                                , register
                                                , silentFallback
                                                )
import           System.Log.Raven.Transport.HttpConduit
                                                ( sendRecord )
import           System.Log.Raven.Types         ( SentryLevel(Error)
                                                , SentryRecord(..)
                                                )
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           System.Logger                  ( Output(StdOut)
                                                , Logger
                                                , create
                                                , info
                                                , debug
                                                , err
                                                )
import           System.Logger.Message          ( msg )
import           TransxchangeParser             ( parseTransxchangeXML )
import           Database                       ( updateTransxchangeData )
import           TransxchangeTypes              ( TransXChangeData )
import           Utility                        ( splitOn )
-- import           Debug.Trace

import qualified Control.Exception             as E
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Vector                   as V

data FTPConnectionDetails = FTPConnectionDetails
  { address  :: String
  , username :: String
  , password :: String
  }

data ServiceReportService = ServiceReportService
  { rowId                :: !Int
  , regionCode           :: !String
  , regionOperatorCode   :: !String
  , serviceCode          :: !String
  , lineName             :: !String
  , description          :: !String
  , startDate            :: !String
  , nationalOperatorCode :: !String
  , dataSource           :: !String
  }
  deriving (Generic, Show)

instance FromNamedRecord ServiceReportService where
  parseNamedRecord r =
    ServiceReportService
      <$> r
      .:  "RowId"
      <*> r
      .:  "RegionCode"
      <*> r
      .:  "RegionOperatorCode"
      <*> r
      .:  "ServiceCode"
      <*> r
      .:  "LineName"
      <*> r
      .:  "Description"
      <*> r
      .:  "StartDate"
      <*> r
      .:  "NationalOperatorCode"
      <*> r
      .:  "DataSource"

main :: IO ()
main = do
  logger <- create StdOut
  info logger (msg @String "Starting fetcher...")
  forever $ do
    info logger (msg @String "Fetching transxchangedata...")
    catch (fetchAndProcessData logger) (handleException logger)
    threadDelay (7 * 1440 * 60 * 1000 * 1000) -- Every 7 days

handleException :: Logger -> SomeException -> IO ()
handleException logger exception = do
  err logger (msg $ "An error occured: " <> show exception)
  sentryDSN     <- getEnv "TRANSXCHANGE_FETCHER_SENTRY_DSN"
  env           <- getEnv "ENVIRONMENT"
  sentryService <- initRaven sentryDSN id sendRecord silentFallback
  register sentryService
           "transxchange-fetcher-logger"
           Error
           (show exception)
           (recordUpdate env)

recordUpdate :: String -> SentryRecord -> SentryRecord
recordUpdate env record = record { srEnvironment = Just env }

fetchAndProcessData :: Logger -> IO ()
fetchAndProcessData logger = do
  ftpAddress  <- getEnv "TRAVELLINE_FTP_ADDRESS"
  ftpUsername <- getEnv "TRAVELLINE_FTP_USERNAME"
  ftpPassword <- getEnv "TRAVELLINE_FTP_PASSWORD"
  let ftpConnectionDetails =
        FTPConnectionDetails ftpAddress ftpUsername ftpPassword
  info logger (msg @String "Downloading servicereport.csv ...")
  downloadFile logger ftpConnectionDetails "servicereport.csv"
  csvData <- BL.readFile "servicereport.csv"
  case decodeByName csvData of
    Left  err           -> error err
    Right (_, services) -> do
      let calmacServices =
            filter (\s -> nationalOperatorCode s == "CALM")
              . V.toList
              $ services
      -- Create a list of files based on the containing zip file. e.g. [(S, [FSACM12, FSACM14]), (SW, [FSACM11, FSACM18])]
      let
        groupedFiles = toList $ fromListWith
          (++)
          [ (regionCode, [serviceCode])
          | ServiceReportService { regionCode = regionCode, serviceCode = serviceCode } <-
            calmacServices
          ]
      allTransxchangeData <- forM groupedFiles
        $ uncurry (downloadAndParseZip logger ftpConnectionDetails)
      updateTransxchangeData $ concat allTransxchangeData
  removeFileIfExists "servicereport.csv"

downloadAndParseZip
  :: Logger
  -> FTPConnectionDetails
  -> String
  -> [String]
  -> IO [TransXChangeData]
downloadAndParseZip logger ftpConnectionDetails zip files = do
  let zipFileName = zip <> ".zip"
  info logger (msg @String $ "Downloading " <> zipFileName <> " ...")
  downloadFile logger ftpConnectionDetails zipFileName
  withArchive zipFileName (unpackInto zip)
  zipTansxchangeData <- forM files $ \file -> do
    let filename = "SVR" <> file <> ".xml"
    info
      logger
      (  msg @String
      $  "Processing "
      <> filename
      <> " in "
      <> zipFileName
      <> " ..."
      )
    fileContents <- readFile $ zip <> "/" <> filename
    return $ parseTransxchangeXML fileContents
  removeFileIfExists zipFileName
  removeDirectoryRecursive zip
  return zipTansxchangeData

downloadFile :: Logger -> FTPConnectionDetails -> String -> IO ()
downloadFile logger (FTPConnectionDetails address username password) file = do
  removeFileIfExists file
  runTCPClient address "21" $ \socket -> do
    welcomeMessage <- recv socket 1024
    debug logger $ msg ("FTP: " <> head (C.split '\r' welcomeMessage))
    sendMessage logger ("USER " <> C.pack username) socket
    sendMessage logger ("PASS " <> C.pack password) socket
    response <- sendMessage logger "PASV" socket
    let (host, port) = extractAddressAndPort $ C.unpack response
    sem <- newQSem 0
    forkIO $ do
      runTCPClient host port $ \transferSocket -> do
        transferData file transferSocket
        signalQSem sem
    sendMessage logger ("RETR " <> C.pack file) socket
    waitQSem sem
    void $ sendMessage logger "QUIT" socket

removeFileIfExists :: String -> IO ()
removeFileIfExists file = do
  fileExists <- doesFileExist file
  when fileExists $ removeFile file

transferData :: String -> Socket -> IO ()
transferData filename socket = do
  response <- recv socket 1024
  C.appendFile filename response
  if C.null response then return () else transferData filename socket

sendMessage :: Logger -> C.ByteString -> Socket -> IO C.ByteString
sendMessage logger message socket = do
  sendAll socket $ message <> "\r\n"
  response <- recv socket 1024
  debug logger $ msg ("FTP: " <> head (C.split '\r' response))
  return response

extractAddressAndPort :: String -> (String, String)
extractAddressAndPort response =
  let [h1, h2, h3, h4, p1, p2] =
        splitOn ',' . init . drop 1 . dropWhile (/= '(') . trim $ response
      host  = intercalate "." [h1, h2, h3, h4]
      port1 = read p1
      port2 = read p2
      port  = show $ (port1 * 256) + port2
  in  (host, port)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
 where
  resolve :: IO AddrInfo
  resolve = do
    let hints = defaultHints { addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) (Just host) (Just port)
  open :: AddrInfo -> IO Socket
  open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    connect sock $ addrAddress addr
    return sock
  openSocket :: AddrInfo -> IO Socket
  openSocket = \addr ->
    socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
