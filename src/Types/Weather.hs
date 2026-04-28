{-# LANGUAGE DeriveGeneric #-}

module Types.Weather where

import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    camelTo2,
    defaultOptions,
    genericParseJSON,
  )
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (Scientific)
import Data.Typeable (Typeable, typeRep)
import GHC.Generics (Generic)

data WeatherFetcherResult = WeatherFetcherResult
  { weatherFetcherResultWeather :: [WeatherFetcherResultWeather],
    weatherFetcherResultMain :: WeatherFetcherResultMain,
    weatherFetcherResultWind :: WeatherFetcherResultWind
  }
  deriving (Generic, Show)

instance FromJSON WeatherFetcherResult where
  parseJSON = genericParseJSON $ weatherFetcherJsonOptions (Proxy :: Proxy WeatherFetcherResult)

data WeatherFetcherResultWeather = WeatherFetcherResultWeather
  { weatherFetcherResultWeatherIcon :: String,
    weatherFetcherResultWeatherDescription :: String
  }
  deriving (Generic, Show)

instance FromJSON WeatherFetcherResultWeather where
  parseJSON = genericParseJSON $ weatherFetcherJsonOptions (Proxy :: Proxy WeatherFetcherResultWeather)

data WeatherFetcherResultMain = WeatherFetcherResultMain
  { weatherFetcherResultMainTemp :: Scientific
  }
  deriving (Generic, Show)

instance FromJSON WeatherFetcherResultMain where
  parseJSON = genericParseJSON $ weatherFetcherJsonOptions (Proxy :: Proxy WeatherFetcherResultMain)

data WeatherFetcherResultWind = WeatherFetcherResultWind
  { weatherFetcherResultWindSpeed :: Scientific,
    weatherFetcherResultWindDeg :: Scientific
  }
  deriving (Generic, Show)

instance FromJSON WeatherFetcherResultWind where
  parseJSON = genericParseJSON $ weatherFetcherJsonOptions (Proxy :: Proxy WeatherFetcherResultWind)

weatherFetcherJsonOptions :: Typeable a => Proxy a -> Options
weatherFetcherJsonOptions type' =
  let typeName = show $ typeRep type'
   in defaultOptions {fieldLabelModifier = camelTo2 '_' . drop (length typeName)}
