{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AWS
  ( EndpointAttributesResult(..)
  , PushPayload(..)
  , createPushEndpoint
  , sendNotificationWihPayload
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
import           Data.Aeson                     ( ToJSON
                                                , toJSON
                                                , (.=)
                                                , encode
                                                , object
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
                                                , Logger
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
import           Data.Binary.Builder            ( Builder
                                                , toLazyByteString
                                                )

import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Encoding       as TE
import qualified System.Logger                 as L
import qualified System.Logger.Message         as L

import           Types

-- { 
--   "default": "This is the default message which must be present when publishing a message to a topic. The default message will only be used if a message is not present for 
-- one of the notification platforms.",     
--   "APNS": "{\"aps\":{\"alert\": \"Check out these awesome deals!\",\"url\":\"www.amazon.com\"} }",
--   "GCM": "{\"data\":{\"message\":\"Check out these awesome deals!\",\"url\":\"www.amazon.com\"}}",
--   "ADM": "{\"data\":{\"message\":\"Check out these awesome deals!\",\"url\":\"www.amazon.com\"}}" 
-- }            
data PushPayload = PushPayload
  { pushPayloadDefault :: String
  , pushPayloadApns :: Maybe String
  , pushPayloadApnsSandbox :: Maybe String
  , pushPayloadGcm :: Maybe String
  } deriving (Show)

instance ToJSON PushPayload where
  toJSON (PushPayload text apns apnsSandbox gcm) = object
    [ "default" .= text
    , "APNS" .= apns
    , "APNS_SANDBOX" .= apnsSandbox
    , "GCM" .= gcm
    ]

data EndpointAttributesResult = EndpointNotFound | AttributeResults String Bool

createPushEndpoint :: L.Logger -> String -> DeviceType -> IO String
createPushEndpoint logger deviceToken deviceType = do
  applePlatformARN <- getEnv "AWS_APPLE_PLATFORM_ARN"
  let request =
        createPlatformEndpoint (pack applePlatformARN) (pack deviceToken)
  result <- performRequestLogging logger request
  return (unpack . fromJust $ result ^. cpersEndpointARN)

getAttributesForEndpoint :: L.Logger -> String -> IO EndpointAttributesResult
getAttributesForEndpoint logger endpointARN = do
  let request = getEndpointAttributes (pack endpointARN)
  result <- trying _NotFoundException (performRequestLogging logger request)
  case result of
    Left  _       -> return EndpointNotFound
    Right result' -> do
      let attributes  = result' ^. gearsAttributes
      let deviceToken = unpack . fromJust $ attributes ^. at "Token"
      let enabled =
            ("true" ==) . (toLower <$>) . unpack . fromJust $ attributes ^. at
              "Enabled"
      return $ AttributeResults deviceToken enabled

updateDeviceTokenForEndpoint :: L.Logger -> String -> String -> IO ()
updateDeviceTokenForEndpoint logger endpointARN deviceToken = do
  let request =
        setEndpointAttributes (pack endpointARN) & seaAttributes .~ fromList
          [("Token", pack deviceToken), ("Enabled", "True")]
  void $ performRequestLogging logger request

sendNotificationWihPayload :: L.Logger -> String -> PushPayload -> IO ()
sendNotificationWihPayload logger endpointARN payload = do
  let pushData = encode payload
  let request =
        publish (T.toStrict . TE.decodeUtf8 $ pushData)
          &  pTargetARN
          ?~ pack endpointARN
          &  pMessageStructure
          ?~ "json"
  void $ performRequestLogging logger request

performRequestLogging :: (AWSRequest a) => L.Logger -> a -> IO (Rs a)
performRequestLogging logger request = do
  lgr    <- createLogger logger
  key_id <- getEnv "AWS_ACCESS_KEY_ID"
  key    <- getEnv "AWS_SECRET_ACCESS_KEY"
  env    <- newEnv
    (FromKeys (AccessKey $ fromString key_id) (SecretKey $ fromString key))
  runResourceT $ runAWS (env & envLogger .~ lgr) $ within Sydney $ send request
 where
  createLogger :: L.Logger -> IO Logger
  createLogger logger = do
    return $ \lvl builder -> do
      case lvl of
        Info  -> L.info logger (L.msg $ toLazyByteString builder)
        Error -> L.err logger (L.msg $ toLazyByteString builder)
        Debug -> L.debug logger (L.msg $ toLazyByteString builder)
        Trace -> L.trace logger (L.msg $ toLazyByteString builder)
