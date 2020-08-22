{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AWS
  ( EndpointAttributesResult(..)
  , createPushEndpoint
  , sendNotification
  , getAttributesForEndpoint
  , updateDeviceTokenForEndpoint
  )
where

import           Control.Lens                   ( (&)
                                                , (.~)
                                                , (?~)
                                                , (^.)
                                                )
import           Control.Lens.At                ( at )
import           Control.Exception.Lens         ( trying )
import           Control.Monad                  ( void )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( pack
                                                , unpack
                                                )
import           Data.Char                      ( toLower )
import           Data.String                    ( fromString )
import           Data.HashMap.Strict            ( fromList )
import           Network.AWS                    ( Credentials(..)
                                                , LogLevel(..)
                                                , Region(..)
                                                , send
                                                , within
                                                , envLogger
                                                , runAWS
                                                , runResourceT
                                                , newEnv
                                                , newLogger
                                                )
import           Network.AWS.Auth               ( AccessKey(..)
                                                , SecretKey(..)
                                                )
import           Network.AWS.SNS                ( _NotFoundException )
import           Network.AWS.SNS.CreatePlatformEndpoint
                                                ( cpersEndpointARN
                                                , createPlatformEndpoint
                                                )
import           Network.AWS.SNS.GetEndpointAttributes
                                                ( getEndpointAttributes
                                                , gearsAttributes
                                                )
import           Network.AWS.SNS.SetEndpointAttributes
                                                ( setEndpointAttributes
                                                , seaAttributes
                                                )
import           Network.AWS.SNS.Publish        ( pMessageStructure
                                                , pTargetARN
                                                , publish
                                                )
import           Network.AWS.Types              ( AWSRequest
                                                , Rs
                                                )
import           System.Environment             ( getEnv )
import           System.IO                      ( stdout )
import           Types

data EndpointAttributesResult = EndpointNotFound | AttributeResults String Bool

createPushEndpoint :: String -> DeviceType -> IO String
createPushEndpoint deviceToken deviceType = do
  applePlatformARN <- getEnv "AWS_APPLE_PLATFORM_ARN"
  let request =
        createPlatformEndpoint (pack applePlatformARN) (pack deviceToken)
  result <- performRequest request
  return (unpack . fromJust $ result ^. cpersEndpointARN)

getAttributesForEndpoint :: String -> IO EndpointAttributesResult
getAttributesForEndpoint endpointARN = do
  let request = getEndpointAttributes (pack endpointARN)
  result <- trying _NotFoundException (performRequest request)
  case result of
    Left  _       -> return EndpointNotFound
    Right result' -> do
      let attributes  = result' ^. gearsAttributes
      let deviceToken = unpack . fromJust $ attributes ^. at "Token"
      let enabled =
            ("true" ==) . (toLower <$>) . unpack . fromJust $ attributes ^. at
              "Enabled"
      return $ AttributeResults deviceToken enabled

updateDeviceTokenForEndpoint :: String -> String -> IO ()
updateDeviceTokenForEndpoint endpointARN deviceToken = do
  let request =
        setEndpointAttributes (pack endpointARN) & seaAttributes .~ fromList
          [("Token", pack deviceToken), ("Enabled", "True")]
  void $ performRequest request

sendNotification :: String -> String -> IO ()
sendNotification endpointARN message = do
  let request = publish (pack message) & pTargetARN ?~ pack endpointARN
  void $ performRequest request

performRequest :: (AWSRequest a) => a -> IO (Rs a)
performRequest request = do
  lgr    <- newLogger Debug stdout
  key_id <- getEnv "AWS_ACCESS_KEY_ID"
  key    <- getEnv "AWS_SECRET_ACCESS_KEY"
  env    <- newEnv
    (FromKeys (AccessKey $ fromString key_id) (SecretKey $ fromString key))
  runResourceT $ runAWS (env & envLogger .~ lgr) $ within Sydney $ send request
