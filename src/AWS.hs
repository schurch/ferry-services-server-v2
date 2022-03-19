{-# LANGUAGE OverloadedStrings #-}

module AWS
  ( EndpointAttributesResult(..)
  , SendNotificationResult(..)
  , PushPayload(..)
  , createPushEndpoint
  , sendNotificationWihPayload
  , getAttributesForEndpoint
  , updateDeviceTokenForEndpoint
  , deletePushEndpoint
  ) where

import           Control.Exception.Lens                 (trying)
import           Control.Lens                           ((&), (.~), (?~), (^.))
import           Control.Lens.At                        (at)
import           Control.Monad                          (void)
import           Data.Aeson                             (encode)
import           Data.Binary.Builder                    (Builder,
                                                         toLazyByteString)
import           Data.Char                              (toLower)
import           Data.HashMap.Strict                    (fromList)
import           Data.Maybe                             (fromJust)
import           Data.String                            (fromString)
import           Data.Text                              (pack, unpack)
import           Network.AWS                            (Credentials (..),
                                                         LogLevel (..), Logger,
                                                         Region (..), envLogger,
                                                         newEnv, newLogger,
                                                         runAWS, runResourceT,
                                                         send, within)
import           Network.AWS.Auth                       (AccessKey (..),
                                                         SecretKey (..))
import           Network.AWS.SNS                        (_EndpointDisabledException,
                                                         _NotFoundException)
import           Network.AWS.SNS.CreatePlatformEndpoint (cpersEndpointARN,
                                                         createPlatformEndpoint)
import           Network.AWS.SNS.DeleteEndpoint         (deleteEndpoint)
import           Network.AWS.SNS.GetEndpointAttributes  (gearsAttributes,
                                                         getEndpointAttributes)
import           Network.AWS.SNS.Publish                (pMessageStructure,
                                                         pTargetARN, publish)
import           Network.AWS.SNS.SetEndpointAttributes  (seaAttributes,
                                                         setEndpointAttributes)
import           Network.AWS.Types                      (AWSRequest, Rs)
import           System.Environment                     (getEnv)
import           System.IO                              (stdout)

import qualified Data.Text.Lazy                         as T
import qualified Data.Text.Lazy.Encoding                as TE
import qualified System.Logger                          as L
import qualified System.Logger.Message                  as L

import           Types                                  (DeviceType (..),
                                                         PushPayload (..))

data EndpointAttributesResult = EndpointAttributesEndpointNotFound | AttributeResults String Bool
data SendNotificationResult = SendNotificationResultSuccess | SendNotificationEndpointDisabled

createPushEndpoint :: L.Logger -> String -> DeviceType -> IO String
createPushEndpoint logger deviceToken deviceType = do
  platformARN <- case deviceType of
    IOS     -> getEnv "AWS_APPLE_PLATFORM_ARN"
    Android -> getEnv "AWS_GOOGLE_PLATFORM_ARN"
  let request = createPlatformEndpoint (pack platformARN) (pack deviceToken)
  result <- performRequestLogging logger request
  return (unpack . fromJust $ result ^. cpersEndpointARN)

deletePushEndpoint :: L.Logger -> String -> IO ()
deletePushEndpoint logger endpointARN = do
  let request = deleteEndpoint (pack endpointARN)
  void $ performRequestLogging logger request

getAttributesForEndpoint :: L.Logger -> String -> IO EndpointAttributesResult
getAttributesForEndpoint logger endpointARN = do
  let request = getEndpointAttributes (pack endpointARN)
  result <- trying _NotFoundException (performRequestLogging logger request)
  case result of
    Left  _       -> return EndpointAttributesEndpointNotFound
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

sendNotificationWihPayload
  :: L.Logger -> String -> PushPayload -> IO SendNotificationResult
sendNotificationWihPayload logger endpointARN payload = do
  let pushData = encode payload
  let request =
        publish (T.toStrict . TE.decodeUtf8 $ pushData)
          &  pTargetARN
          ?~ pack endpointARN
          &  pMessageStructure
          ?~ "json"
  result <- trying _EndpointDisabledException
    $ performRequestLogging logger request
  case result of
    Left  _ -> return SendNotificationEndpointDisabled
    Right _ -> return SendNotificationResultSuccess

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
