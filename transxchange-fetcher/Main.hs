{-# LANGUAGE OverloadedStrings #-}

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
import           System.Directory
import           System.Environment             ( getEnv )
import           Control.Monad                  ( when
                                                , void
                                                )

import qualified Control.Exception             as E
import qualified Data.ByteString.Char8         as C

main :: IO ()
main = do
  ftpAddress  <- getEnv "TRAVELLINE_FTP_ADDRESS"
  ftpUsername <- getEnv "TRAVELLINE_FTP_USERNAME"
  ftpPassword <- getEnv "TRAVELLINE_FTP_PASSWORD"
  downloadFile ftpAddress ftpUsername ftpPassword "servicereport.csv"
  downloadFile ftpAddress ftpUsername ftpPassword "S.zip"

downloadFile :: String -> String -> String -> String -> IO ()
downloadFile address username password filename = do
  fileExists <- doesFileExist filename
  when fileExists $ removeFile filename
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
        transferData filename transferSocket
        signalQSem sem
    sendMessage ("RETR " <> C.pack filename) socket
    waitQSem sem
    void $ sendMessage "QUIT" socket

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
