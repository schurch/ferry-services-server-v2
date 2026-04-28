{-# LANGUAGE DeriveGeneric #-}

module Types.Vessel where

import Data.Aeson
  ( FromJSON (parseJSON),
    defaultOptions,
    fieldLabelModifier,
    genericParseJSON,
  )
import Data.Char (toUpper)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (typeRep)
import GHC.Generics (Generic)

data AjaxVessel = AjaxVessel
  { ajaxVesselMmsi :: String,
    ajaxVesselShipname :: String,
    ajaxVesselLat :: String,
    ajaxVesselLon :: String,
    ajaxVesselSpeed :: Maybe String,
    ajaxVesselCourse :: Maybe String,
    ajaxVesselTimestamp :: String
  }
  deriving (Generic, Show)

instance FromJSON AjaxVessel where
  parseJSON =
    genericParseJSON $
      defaultOptions
        { fieldLabelModifier = map toUpper . drop (length . show . typeRep $ (Proxy :: Proxy AjaxVessel))
        }
