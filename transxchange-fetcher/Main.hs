{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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
                                                )
import           System.Directory               ( doesFileExist
                                                , removeFile
                                                , removeDirectoryRecursive
                                                )
import           System.Environment             ( getEnv )
import           Control.Monad                  ( when
                                                , void
                                                , forM_
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

import qualified Control.Exception             as E
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Vector                   as V

data ServiceReportService = ServiceReportService {
    rowId :: !Int
  , regionCode :: !String
  , regionOperatorCode :: !String
  , serviceCode :: !String
  , lineName :: !String
  , description :: !String
  , startDate :: !String
  , nationalOperatorCode :: !String
  , dataSource :: !String
} deriving (Generic, Show)

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
  ftpAddress  <- getEnv "TRAVELLINE_FTP_ADDRESS"
  ftpUsername <- getEnv "TRAVELLINE_FTP_USERNAME"
  ftpPassword <- getEnv "TRAVELLINE_FTP_PASSWORD"
  putStrLn "Downloading servicereport.csv ..."
  downloadFile ftpAddress ftpUsername ftpPassword "servicereport.csv"
  csvData <- BL.readFile "servicereport.csv"
  case decodeByName csvData of
    Left  err           -> putStrLn err
    Right (_, services) -> do
      let calmacServices =
            filter (\s -> nationalOperatorCode s == "CALM")
              . V.toList
              $ services
      let files = (\s -> (regionCode s, serviceCode s)) <$> calmacServices
      -- Create a list of files based on the containing zip file. e.g. [(S, [FSACM12, FSACM14]), (SW, [FSACM11, FSACM18])]
      let groupedFiles =
            toList $ fromListWith (++) [ (k, [v]) | (k, v) <- files ]
      forM_ groupedFiles $ \(zip, files) -> do
        let zipFileName = zip <> ".zip"
        putStrLn $ "Downloading " <> zipFileName <> " ..."
        downloadFile ftpAddress ftpUsername ftpPassword zipFileName
        putStrLn $ "Processing " <> zipFileName <> " ..."
        withArchive zipFileName (unpackInto zip)
        -- removeFileIfExists zipFileName
        removeDirectoryRecursive zip
        putStrLn $ "Finished processing " <> zipFileName
  -- removeFileIfExists "servicereport.csv"
  putStrLn "Completed processing"

downloadFile :: String -> String -> String -> String -> IO ()
downloadFile address username password file = do
  removeFileIfExists file
  runTCPClient address "21" $ \socket -> do
    msg <- recv socket 1024
    C.putStrLn msg
    sendMessage ("USER " <> C.pack username) socket
    sendMessage ("PASS " <> C.pack password) socket
    response <- sendMessage "PASV" socket
    let (host, port) = extractAddressAndPort $ C.unpack response
    sem <- newQSem 0
    forkIO $ do
      runTCPClient host port $ \transferSocket -> do
        transferData file transferSocket
        signalQSem sem
    sendMessage ("RETR " <> C.pack file) socket
    waitQSem sem
    void $ sendMessage "QUIT" socket

removeFileIfExists :: String -> IO ()
removeFileIfExists file = do
  fileExists <- doesFileExist file
  when fileExists $ removeFile file

transferData :: String -> Socket -> IO ()
transferData filename socket = do
  response <- recv socket 1024
  C.appendFile filename response
  if C.null response then return () else transferData filename socket

sendMessage :: C.ByteString -> Socket -> IO C.ByteString
sendMessage message socket = do
  sendAll socket $ message <> "\r\n"
  response <- recv socket 1024
  C.putStrLn response
  return response

extractAddressAndPort :: String -> (String, String)
extractAddressAndPort response =
  let [h1, h2, h3, h4, p1, p2] =
          splitBy ',' . init . drop 1 . dropWhile (/= '(') . trim $ response
      host  = intercalate "." [h1, h2, h3, h4]
      port1 = read p1
      port2 = read p2
      port  = show $ (port1 * 256) + port2
  in  (host, port)

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
 where
  f c l@(x : xs) | c == delimiter = [] : l
                 | otherwise      = (c : x) : xs

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
