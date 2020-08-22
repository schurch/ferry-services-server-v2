{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AWS
  ( createPushEndpoint
  , sendNotification
  )
where

import           Control.Lens                   ( (&)
                                                , (.~)
                                                , (?~)
                                                , (^.)
                                                )
import           Control.Monad                  ( void )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( pack
                                                , unpack
                                                )
import           Data.String                    ( fromString )
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
import           Network.AWS.SNS.CreatePlatformEndpoint
                                                ( cpersEndpointARN
                                                , createPlatformEndpoint
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


createPushEndpoint :: String -> IO String
createPushEndpoint deviceToken = do
  applePlatformARN <- getEnv "AWS_APPLE_PLATFORM_ARN"
  let request =
        createPlatformEndpoint (pack applePlatformARN) (pack deviceToken)
  result <- performRequest request
  return (unpack . fromJust $ result ^. cpersEndpointARN)

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
