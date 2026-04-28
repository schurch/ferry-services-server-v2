{-# LANGUAGE DeriveGeneric #-}

module Types.Rail where

import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    defaultOptions,
    genericParseJSON,
  )
import Data.Char (toLower)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep)
import GHC.Generics (Generic)

data RailDepartureFetcherResult = RailDepartureFetcherResult
  { railDepartureFetcherResultLocationName :: String,
    railDepartureFetcherResultCrs :: String,
    railDepartureFetcherResultTrainServices :: Maybe [RailDepartureFetcherTrainService]
  }
  deriving (Generic, Show)

instance FromJSON RailDepartureFetcherResult where
  parseJSON = genericParseJSON $ railDepartureFetcherJsonOptions (Proxy :: Proxy RailDepartureFetcherResult)

data RailDepartureFetcherTrainService = RailDepartureFetcherTrainService
  { railDepartureFetcherTrainServiceDestination :: [RailDepartureFetcherLocation],
    railDepartureFetcherTrainServiceCurrentDestinations :: Maybe [RailDepartureFetcherLocation],
    railDepartureFetcherTrainServiceStd :: String,
    railDepartureFetcherTrainServiceEtd :: String,
    railDepartureFetcherTrainServicePlatform :: Maybe String,
    railDepartureFetcherTrainServiceIsCancelled :: Bool
  }
  deriving (Generic, Show)

instance FromJSON RailDepartureFetcherTrainService where
  parseJSON = genericParseJSON $ railDepartureFetcherJsonOptions (Proxy :: Proxy RailDepartureFetcherTrainService)

data RailDepartureFetcherLocation = RailDepartureFetcherLocation
  { railDepartureFetcherLocationLocationName :: String,
    railDepartureFetcherLocationCrs :: String
  }
  deriving (Generic, Show)

instance FromJSON RailDepartureFetcherLocation where
  parseJSON = genericParseJSON $ railDepartureFetcherJsonOptions (Proxy :: Proxy RailDepartureFetcherLocation)

railDepartureFetcherJsonOptions :: Typeable a => Proxy a -> Options
railDepartureFetcherJsonOptions type' =
  let typeName = show $ typeRep type'
   in defaultOptions {fieldLabelModifier = toLowerFirstLetter . drop (length typeName)}

toLowerFirstLetter :: String -> String
toLowerFirstLetter [] = []
toLowerFirstLetter (x : xs) = toLower x : xs
