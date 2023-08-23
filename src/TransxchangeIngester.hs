{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TransxchangeIngester (ingest) where

import Codec.Archive.Zip
  ( unpackInto,
    withArchive,
  )
import Control.Concurrent
  ( forkIO,
    newQSem,
    signalQSem,
    threadDelay,
    waitQSem,
  )
import Control.Exception (finally, tryJust)
import qualified Control.Exception as E
import Control.Monad
  ( forM,
    forever,
    guard,
    void,
    when,
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.Csv
  ( FromNamedRecord (..),
    decodeByName,
    (.:),
  )
import Data.List
  ( intercalate,
  )
import Data.Map
  ( fromListWith,
    toList,
  )
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Database (updateTransxchangeData)
import GHC.Generics (Generic)
import Network.Socket
  ( AddrInfo
      ( addrAddress,
        addrFamily,
        addrProtocol,
        addrSocketType
      ),
    HostName,
    ServiceName,
    Socket,
    SocketType (Stream),
    close,
    connect,
    defaultHints,
    getAddrInfo,
    socket,
    withSocketsDo,
  )
import Network.Socket.ByteString
  ( recv,
    sendAll,
  )
import System.Directory
  ( doesFileExist,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)
import System.Logger (Level (..), log)
import System.Logger.Class (Logger, debug, info)
import System.Logger.Message (msg)
import TransxchangeParser (parseTransxchangeXML)
import TransxchangeTypes (TransXChangeData (TransXChangeData))
import Types (Application, Env (..))
import Utility (splitOn, trim)

data FTPConnectionDetails = FTPConnectionDetails
  { address :: String,
    username :: String,
    password :: String
  }

data ServiceReportService = ServiceReportService
  { rowId :: !Int,
    regionCode :: !String,
    regionOperatorCode :: !String,
    serviceCode :: !String,
    lineName :: !String,
    description :: !String,
    startDate :: !String,
    nationalOperatorCode :: !String,
    dataSource :: !String
  }
  deriving (Generic, Show)

instance FromNamedRecord ServiceReportService where
  parseNamedRecord r =
    ServiceReportService
      <$> r
      .: "RowId"
      <*> r
      .: "RegionCode"
      <*> r
      .: "RegionOperatorCode"
      <*> r
      .: "ServiceCode"
      <*> r
      .: "LineName"
      <*> r
      .: "Description"
      <*> r
      .: "StartDate"
      <*> r
      .: "NationalOperatorCode"
      <*> r
      .: "DataSource"

ingest :: Application ()
ingest = do
  ftpAddress <- liftIO $ getEnv "TRAVELLINE_FTP_ADDRESS"
  ftpUsername <- liftIO $ getEnv "TRAVELLINE_FTP_USERNAME"
  ftpPassword <- liftIO $ getEnv "TRAVELLINE_FTP_PASSWORD"
  logger <- asks logger
  let ftpConnectionDetails =
        FTPConnectionDetails ftpAddress ftpUsername ftpPassword
  liftIO $ downloadFile logger ftpConnectionDetails "servicereport.csv"
  csvData <- liftIO $ BL.readFile "servicereport.csv"
  case decodeByName csvData of
    Left err -> error err
    Right (_, services) -> do
      let servicesToIngest =
            filter (\s -> nationalOperatorCode s `elem` ["CALM", "NLKF", "WFRL", "SHET", "OKFL"])
              . V.toList
              $ services
      -- Create a list of files based on the containing zip file. e.g. [(S, [FSACM12, FSACM14]), (SW, [FSACM11, FSACM18])]
      let groupedFiles =
            toList $
              fromListWith
                (++)
                [ (regionCode, [serviceCode])
                  | ServiceReportService {regionCode = regionCode, serviceCode = serviceCode} <-
                      servicesToIngest
                ]
      allTransxchangeData <-
        forM groupedFiles $
          uncurry (downloadAndParseZip ftpConnectionDetails)
      updateTransxchangeData $ concat allTransxchangeData
  liftIO $ removeFileIfExists "servicereport.csv"
  info (msg @String "Done")

downloadAndParseZip ::
  FTPConnectionDetails ->
  String ->
  [String] ->
  Application [TransXChangeData]
downloadAndParseZip ftpConnectionDetails zip files = do
  logger <- asks logger
  let zipFileName = zip <> ".zip"
  info (msg @String $ "Downloading " <> zipFileName <> " ...")
  liftIO $ finally (downloadAndParse logger zipFileName) (cleanup zipFileName)
  where
    downloadAndParse :: Logger -> String -> IO [TransXChangeData]
    downloadAndParse logger zipFileName = do
      liftIO $ downloadFile logger ftpConnectionDetails zipFileName
      withArchive zipFileName (unpackInto zip)
      result <- forM files $ \file -> do
        let filename = "SVR" <> file <> ".xml"
        System.Logger.log
          logger
          Info
          ( msg @String $
              "Processing "
                <> filename
                <> " in "
                <> zipFileName
                <> " ..."
          )
        fileContents <- tryJust (guard . isDoesNotExistError) $ readFile $ zip <> "/" <> filename
        case fileContents of
          Left _ -> do
            System.Logger.log logger Error (msg @String $ filename <> " missing")
            return Nothing
          Right fileContents' -> return . Just . parseTransxchangeXML $ fileContents'
      return $ catMaybes result

    cleanup :: String -> IO ()
    cleanup zipFileName = removeFileIfExists zipFileName >> removeDirectoryRecursive zip

downloadFile :: Logger -> FTPConnectionDetails -> String -> IO ()
downloadFile logger (FTPConnectionDetails address username password) file = do
  removeFileIfExists file
  runTCPClient address "21" $ \socket -> do
    welcomeMessage <- recv socket 1024
    System.Logger.log logger Debug $ msg ("FTP: " <> head (C.split '\r' welcomeMessage))
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
  response <- liftIO $ recv socket 1024
  System.Logger.log logger Debug $ msg ("FTP: " <> head (C.split '\r' response))
  return response

extractAddressAndPort :: String -> (String, String)
extractAddressAndPort response =
  let [h1, h2, h3, h4, p1, p2] =
        splitOn ',' . init . drop 1 . dropWhile (/= '(') . trim $ response
      host = intercalate "." [h1, h2, h3, h4]
      port1 = read p1
      port2 = read p2
      port = show $ (port1 * 256) + port2
   in (host, port)

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
  where
    resolve :: IO AddrInfo
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open :: AddrInfo -> IO Socket
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock
    openSocket :: AddrInfo -> IO Socket
    openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)