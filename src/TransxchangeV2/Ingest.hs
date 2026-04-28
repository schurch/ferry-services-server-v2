{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TransxchangeV2.Ingest
  ( ingestDirectoryV2,
    ingestLatestV2,
  )
where

import Codec.Archive.Zip
  ( unpackInto,
    withArchive,
  )
import Control.Concurrent
  ( forkIO,
    newQSem,
    signalQSem,
    waitQSem,
  )
import Control.Exception (finally)
import qualified Control.Exception as E
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks, runReaderT)
import qualified Data.ByteString.Char8 as C
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (withTransaction)
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
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Environment (getEnv)
import App.Logger
  ( Level (Debug, Info),
    Logger,
    logInfoM,
    logMessage,
  )
import TransxchangeV2.Parser
  ( findTransxchangeFilesV2,
    fileMayContainFerryServiceV2,
    parseTransxchangeFileV2,
  )
import TransxchangeV2.Repository
  ( clearTx2Tables,
    insertTx2Document,
  )
import TransxchangeV2.Transform (filterFerryDocuments)
import TransxchangeV2.Types
import Types (Application, Env (connectionPool, logger))
import Utility (splitOn, trim)

data FTPConnectionDetails = FTPConnectionDetails
  { address :: String,
    username :: String,
    password :: String
  }

ingestLatestV2 :: Application Tx2IngestSummary
ingestLatestV2 = do
  ftpAddress <- liftIO $ getEnv "TRAVELLINE_FTP_ADDRESS"
  ftpUsername <- liftIO $ getEnv "TRAVELLINE_FTP_USERNAME"
  ftpPassword <- liftIO $ getEnv "TRAVELLINE_FTP_PASSWORD"
  appLogger <- asks logger
  env <- ask
  let ftpConnectionDetails =
        FTPConnectionDetails ftpAddress ftpUsername ftpPassword
  logInfoM "Downloading S.zip for TransXChange V2 ingest ..."
  liftIO $
    finally
      (downloadAndIngest appLogger ftpConnectionDetails env)
      cleanupDownload
  where
    zipFileName = "S.zip"
    extractDirectory = "S"

    downloadAndIngest :: Logger -> FTPConnectionDetails -> Env -> IO Tx2IngestSummary
    downloadAndIngest appLogger ftpConnectionDetails env = do
      removeFileIfExists zipFileName
      removeDirectoryIfExists extractDirectory
      createDirectoryIfMissing True extractDirectory
      downloadFile appLogger ftpConnectionDetails zipFileName
      withArchive zipFileName $ unpackInto extractDirectory
      runReaderT (ingestDirectoryV2 extractDirectory) env

    cleanupDownload :: IO ()
    cleanupDownload = do
      removeFileIfExists zipFileName
      removeDirectoryIfExists extractDirectory

ingestDirectoryV2 :: FilePath -> Application Tx2IngestSummary
ingestDirectoryV2 directory = do
  logInfoM $ "TransXChange V2 ingest from directory: " <> directory
  files <- liftIO $ findTransxchangeFilesV2 directory
  logInfoM $ "TransXChange V2 files discovered: " <> show (length files)
  pool <- asks connectionPool
  appLogger <- asks logger
  liftIO $
    withResource pool $ \connection ->
      withTransaction connection $ do
        clearTx2Tables connection
        go appLogger connection emptySummary 1 (length files) files
  where
    emptySummary =
      Tx2IngestSummary
        { tx2FilesParsed = 0,
          tx2FilesSkipped = 0,
          tx2FilesFailed = 0,
          tx2FerryDocuments = 0,
          tx2ServicesWritten = 0,
          tx2JourneyWritten = 0
        }

    go _ _ !summary _ _ [] = return summary
    go appLogger connection !summary !index !totalFiles (filePath : remainingFiles) = do
      logProgress appLogger index totalFiles filePath summary
      mayContainFerry <- fileMayContainFerryServiceV2 filePath
      if not mayContainFerry
        then do
          let !nextSummary =
                summary
                  { tx2FilesSkipped = tx2FilesSkipped summary + 1
                  }
          go appLogger connection nextSummary (index + 1) totalFiles remainingFiles
        else do
          parsed <- parseTransxchangeFileV2 filePath
          case parsed of
            Left _ ->
              let !nextSummary =
                    summary
                      { tx2FilesFailed = tx2FilesFailed summary + 1
                      }
               in go appLogger connection nextSummary (index + 1) totalFiles remainingFiles
            Right document -> do
              let ferryDocuments = filterFerryDocuments [document]
              mapM_ (insertTx2Document connection) ferryDocuments
              let ferryDocumentCount = length ferryDocuments
              let serviceCount = sum $ fmap (length . tx2Services) ferryDocuments
              let journeyCount = sum $ fmap (length . tx2VehicleJourneys) ferryDocuments
              let !nextSummary =
                    summary
                      { tx2FilesParsed = tx2FilesParsed summary + 1,
                        tx2FerryDocuments = tx2FerryDocuments summary + ferryDocumentCount,
                        tx2ServicesWritten = tx2ServicesWritten summary + serviceCount,
                        tx2JourneyWritten = tx2JourneyWritten summary + journeyCount
                      }
              go appLogger connection nextSummary (index + 1) totalFiles remainingFiles

    logProgress :: Logger -> Int -> Int -> FilePath -> Tx2IngestSummary -> IO ()
    logProgress appLogger index totalFiles filePath summary =
      if shouldLogProgress index totalFiles
        then
          logMessage
            appLogger
            Info
            ( "TransXChange V2 progress "
                <> show index
                <> "/"
                <> show totalFiles
                <> ": "
                <> filePath
                <> " (parsed="
                <> show (tx2FilesParsed summary)
                <> ", skipped="
                <> show (tx2FilesSkipped summary)
                <> ", failed="
                <> show (tx2FilesFailed summary)
                <> ", ferry_documents="
                <> show (tx2FerryDocuments summary)
                <> ", services_written="
                <> show (tx2ServicesWritten summary)
                <> ", vehicle_journeys_written="
                <> show (tx2JourneyWritten summary)
                <> ")"
            )
        else return ()

    shouldLogProgress :: Int -> Int -> Bool
    shouldLogProgress index totalFiles =
      index == 1 || index == totalFiles || index `mod` 50 == 0

downloadFile :: Logger -> FTPConnectionDetails -> FilePath -> IO ()
downloadFile appLogger (FTPConnectionDetails ftpAddress ftpUsername ftpPassword) filePath = do
  removeFileIfExists filePath
  runTCPClient ftpAddress "21" $ \socketHandle -> do
    welcomeMessage <- recv socketHandle 1024
    logMessage appLogger Info $ "FTP: " <> C.unpack (headOrEmpty $ C.split '\r' welcomeMessage)
    sendMessage appLogger ("USER " <> C.pack ftpUsername) socketHandle
    sendMessage appLogger ("PASS " <> C.pack ftpPassword) socketHandle
    passiveResponse <- sendMessage appLogger "PASV" socketHandle
    let (host, port) = extractAddressAndPort $ C.unpack passiveResponse
    semaphore <- newQSem 0
    _ <-
      forkIO $ do
        runTCPClient host port $ \transferSocket -> do
          transferData filePath transferSocket
          signalQSem semaphore
    sendMessage appLogger ("RETR " <> C.pack filePath) socketHandle
    waitQSem semaphore
    _ <- sendMessage appLogger "QUIT" socketHandle
    return ()

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists filePath = do
  fileExists <- doesFileExist filePath
  if fileExists then removeFile filePath else return ()

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists directory = do
  directoryExists <- doesDirectoryExist directory
  if directoryExists then removeDirectoryRecursive directory else return ()

transferData :: FilePath -> Socket -> IO ()
transferData filePath socketHandle = do
  response <- recv socketHandle 1024
  C.appendFile filePath response
  if C.null response then return () else transferData filePath socketHandle

sendMessage :: Logger -> C.ByteString -> Socket -> IO C.ByteString
sendMessage appLogger message socketHandle = do
  sendAll socketHandle $ message <> "\r\n"
  response <- recv socketHandle 1024
  logMessage appLogger Debug $ "FTP: " <> C.unpack (headOrEmpty $ C.split '\r' response)
  return response

extractAddressAndPort :: String -> (String, String)
extractAddressAndPort response =
  let [h1, h2, h3, h4, p1, p2] =
        splitOn ',' . init . drop 1 . dropWhile (/= '(') . trim $ response
      host = h1 <> "." <> h2 <> "." <> h3 <> "." <> h4
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
      addresses <- getAddrInfo (Just hints) (Just host) (Just port)
      case addresses of
        address : _ -> pure address
        [] -> fail $ "No address info found for " <> host <> ":" <> port

    open :: AddrInfo -> IO Socket
    open addr =
      E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

    openSocket :: AddrInfo -> IO Socket
    openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

headOrEmpty :: [C.ByteString] -> C.ByteString
headOrEmpty [] = ""
headOrEmpty (firstValue : _) = firstValue
