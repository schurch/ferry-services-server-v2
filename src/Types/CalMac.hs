{-# LANGUAGE DeriveGeneric #-}

module Types.CalMac where

import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier, omitNothingFields),
    ToJSON (toJSON),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Data.Char (toLower)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep)
import GHC.Generics (Generic)
import Types (jsonOptions)

data CalMacAPIRequestBody = CalMacAPIRequestBody
  { calMacAPIRequestBodyQuery :: String
  }
  deriving (Generic, Show)

instance ToJSON CalMacAPIRequestBody where
  toJSON = genericToJSON $ jsonOptions (Proxy :: Proxy CalMacAPIRequestBody)

data CalMacAPIResponse = CalMacAPIResponse
  { calMacAPIResponseData :: CalMacAPIResponseData
  }
  deriving (Generic, Show)

instance FromJSON CalMacAPIResponse where
  parseJSON = genericParseJSON $ calMacAPIResponseJsonOptions (Proxy :: Proxy CalMacAPIResponse)

data CalMacAPIResponseData = CalMacAPIResponseData
  { calMacAPIResponseDataRoutes :: [CalMacAPIResponseRoute]
  }
  deriving (Generic, Show)

instance FromJSON CalMacAPIResponseData where
  parseJSON = genericParseJSON $ calMacAPIResponseJsonOptions (Proxy :: Proxy CalMacAPIResponseData)

data CalMacAPIResponseRoute = CalMacAPIResponseRoute
  { calMacAPIResponseRouteName :: String,
    calMacAPIResponseRouteStatus :: String,
    calMacAPIResponseRouteRouteCode :: String,
    calMacAPIResponseRouteLocation :: CalMacAPIResponseRouteLocation,
    calMacAPIResponseRouteRouteStatuses :: [CalMacAPIResponseRouteStatus]
  }
  deriving (Generic, Show)

instance FromJSON CalMacAPIResponseRoute where
  parseJSON = genericParseJSON $ calMacAPIResponseJsonOptions (Proxy :: Proxy CalMacAPIResponseRoute)

data CalMacAPIResponseRouteLocation = CalMacAPIResponseRouteLocation
  { calMacAPIResponseRouteLocationName :: String
  }
  deriving (Generic, Show)

instance FromJSON CalMacAPIResponseRouteLocation where
  parseJSON = genericParseJSON $ calMacAPIResponseJsonOptions (Proxy :: Proxy CalMacAPIResponseRouteLocation)

data CalMacAPIResponseRouteStatus = CalMacAPIResponseRouteStatus
  { calMacAPIResponseRouteStatusTitle :: String,
    calMacAPIResponseRouteStatusStatus :: String,
    calMacAPIResponseRouteStatusDetail :: String
  }
  deriving (Generic, Show)

instance FromJSON CalMacAPIResponseRouteStatus where
  parseJSON = genericParseJSON $ calMacAPIResponseJsonOptions (Proxy :: Proxy CalMacAPIResponseRouteStatus)

calMacAPIResponseJsonOptions :: Typeable a => Proxy a -> Options
calMacAPIResponseJsonOptions type' =
  let typeName = show $ typeRep type'
   in defaultOptions
        { fieldLabelModifier = toLowerFirstLetter . drop (length typeName),
          omitNothingFields = True
        }

toLowerFirstLetter :: String -> String
toLowerFirstLetter [] = []
toLowerFirstLetter (x : xs) = toLower x : xs
