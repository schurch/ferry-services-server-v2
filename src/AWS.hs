{-# LANGUAGE OverloadedStrings #-}

module AWS
  ( EndpointAttributesResult (..),
    SendNotificationResult (..),
    PushPayload (..),
    createPushEndpoint,
    sendNotificationWihPayload,
    getAttributesForEndpoint,
    updateDeviceTokenForEndpoint,
    deletePushEndpoint,
  )
where

import Amazonka
  ( AWSRequest,
    AWSResponse,
    LogLevel (..),
    Logger,
    Region (Sydney),
    newEnv,
    runResourceT,
    send,
  )
import Amazonka.Auth
  ( AccessKey (..),
    SecretKey (..),
    envLogger,
    envRegion,
    fromKeys,
  )
import Amazonka.SNS
  ( _EndpointDisabledException,
    _NotFoundException,
  )
import Amazonka.SNS.CreatePlatformEndpoint
  ( createPlatformEndpointResponse_endpointArn,
    newCreatePlatformEndpoint,
  )
import Amazonka.SNS.DeleteEndpoint (newDeleteEndpoint)
import Amazonka.SNS.GetEndpointAttributes
  ( getEndpointAttributesResponse_attributes,
    newGetEndpointAttributes,
  )
import Amazonka.SNS.Publish
  ( newPublish,
    publish_messageStructure,
    publish_targetArn,
  )
import Amazonka.SNS.SetEndpointAttributes
  ( newSetEndpointAttributes,
    setEndpointAttributes_attributes,
  )
import Control.Exception.Lens (trying)
import Control.Lens ((&), (.~), (?~), (^.))
import Control.Lens.At (at)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (encode)
import Data.Binary.Builder (Builder, toLazyByteString)
import Data.Char (toLower)
import Data.HashMap.Strict (fromList)
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text (pack, unpack)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import System.Environment (getEnv)
import System.IO (stdout)
import qualified System.Logger as L
import qualified System.Logger.Message as L
import Types
  ( DeviceType (..),
    PushPayload (..),
  )

data EndpointAttributesResult = EndpointAttributesEndpointNotFound | AttributeResults String Bool

data SendNotificationResult = SendNotificationResultSuccess | SendNotificationEndpointDisabled

createPushEndpoint :: L.Logger -> String -> DeviceType -> IO String
createPushEndpoint logger deviceToken deviceType = do
  platformARN <- case deviceType of
    IOS -> getEnv "AWS_APPLE_PLATFORM_ARN"
    Android -> getEnv "AWS_GOOGLE_PLATFORM_ARN"
  let request = newCreatePlatformEndpoint (pack platformARN) (pack deviceToken)
  result <- performRequestLogging logger request
  return (unpack . fromJust $ result ^. createPlatformEndpointResponse_endpointArn)

deletePushEndpoint :: L.Logger -> String -> IO ()
deletePushEndpoint logger endpointARN = do
  let request = newDeleteEndpoint (pack endpointARN)
  void $ performRequestLogging logger request

getAttributesForEndpoint :: L.Logger -> String -> IO EndpointAttributesResult
getAttributesForEndpoint logger endpointARN = do
  let request = newGetEndpointAttributes (pack endpointARN)
  result <- trying _NotFoundException (performRequestLogging logger request)
  case result of
    Left _ -> return EndpointAttributesEndpointNotFound
    Right result' -> do
      let attributes = fromJust $ result' ^. getEndpointAttributesResponse_attributes
      let deviceToken = unpack . fromJust $ attributes ^. at "Token"
      let enabled =
            ("true" ==) . (toLower <$>) . unpack . fromJust $
              attributes
                ^. at
                  "Enabled"
      return $ AttributeResults deviceToken enabled

updateDeviceTokenForEndpoint :: L.Logger -> String -> String -> IO ()
updateDeviceTokenForEndpoint logger endpointARN deviceToken = do
  let request =
        newSetEndpointAttributes (pack endpointARN)
          & setEndpointAttributes_attributes
            .~ fromList
              [("Token", pack deviceToken), ("Enabled", "True")]
  void $ performRequestLogging logger request

sendNotificationWihPayload ::
  L.Logger -> String -> PushPayload -> IO SendNotificationResult
sendNotificationWihPayload logger endpointARN payload = do
  let pushData = encode payload
  let request =
        newPublish (T.toStrict . TE.decodeUtf8 $ pushData)
          & publish_targetArn
            ?~ pack endpointARN
          & publish_messageStructure
            ?~ "json"
  result <-
    trying _EndpointDisabledException $
      performRequestLogging logger request
  case result of
    Left _ -> return SendNotificationEndpointDisabled
    Right _ -> return SendNotificationResultSuccess

performRequestLogging :: (AWSRequest a) => L.Logger -> a -> IO (AWSResponse a)
performRequestLogging logger request = do
  lgr <- createLogger logger
  key_id <- getEnv "AWS_ACCESS_KEY_ID"
  key <- getEnv "AWS_SECRET_ACCESS_KEY"
  keysEnv <- newEnv $ pure . fromKeys (AccessKey $ fromString key_id) (SecretKey $ fromString key)
  let env =
        keysEnv
          { envLogger = lgr,
            envRegion = Sydney
          }
  runResourceT $ send env request
  where
    createLogger :: L.Logger -> IO Logger
    createLogger logger = do
      return $ \lvl builder -> do
        case lvl of
          Info -> L.info logger (L.msg $ toLazyByteString builder)
          Error -> L.err logger (L.msg $ toLazyByteString builder)
          Debug -> L.debug logger (L.msg $ toLazyByteString builder)
          Trace -> L.trace logger (L.msg $ toLazyByteString builder)
