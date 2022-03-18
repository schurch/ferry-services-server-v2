{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module JSONSpec where

import           Data.Aeson                     (encode)
import           Data.Aeson.QQ                  (aesonQQ)
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Clock
import           Test.Hspec                     (Spec, describe, it, shouldBe)

import           Types

spec :: Spec
spec = do
  describe "Notification Payloads" $ do
    describe "APSPayload" $
      it "encodes correctly" $
        encode APSPayload
        { apsPayloadAps = APSPayloadBody
          { apsPayloadBodyAlert = "There is a disruption to the Arran service"
          , apsPayloadBodySound = "default"
          }
        , apsPayloadServiceID = 5
        }
        `shouldBe`
        encode [aesonQQ|
          {
            "service_id": 5,
            "aps": {
                "sound": "default",
                "alert": "There is a disruption to the Arran service"
            }
          }
        |]

    describe "CGMPayload" $
      it "encodes correctly" $
        encode CGMPayload
        { gcmPayloadData = GCMPayloadData
          { gcmPayloadDataServiceID = 5
          , gcmPayloadDataTitle = "Sailings disrupted"
          , gcmPayloadDataBody = "Ardrossan (ARD) - Brodick (BRO)"
          }
        }
        `shouldBe`
        encode [aesonQQ|
          {
            "data": {
              "service_id": 5,
              "title": "Sailings disrupted",
              "body": "Ardrossan (ARD) - Brodick (BRO)"
            }
          }
        |]

  describe "API payloads" $ do
    describe "ServiceResponse" $
      it "encodes correctly" $
        encode ServiceResponse
        { serviceResponseServiceID = 5
        , serviceResponseSortOrder = 1
        , serviceResponseArea = "ARRAN"
        , serviceResponseRoute = "Ardrossan (ARD) - Brodick (BRO)"
        , serviceResponseStatus = Disrupted
        , serviceResponseLocations = [
          LocationResponse
          { locationResponseID = 4
          , locationResponseName = "Brodick"
          , locationResponseLatitude = 55.576606
          , locationResponseLongitude = -5.139172
          , locationResponseWeather = Just LocationWeatherResponse
          { locationWeatherResponseIcon = "10n"
          , locationWeatherResponseDescription = "Moderate rain"
          , locationWeatherResponseTemperatureCelsius = 5
          , locationWeatherResponseWindSpeedMPH = 18
          , locationWeatherResponseWindDirection = 196.4
          , locationWeatherResponseWindDirectionCardinal = "SSW"
          }
          }
        ]
        , serviceResponseAdditionalInfo = Just "Additional info"
        , serviceResponseDisruptionReason = Just "Weather"
        , serviceResponseLastUpdatedDate =  Just $ UTCTime (fromOrdinalDate 2022 32) 5000
        , serviceResponseUpdated = UTCTime (fromOrdinalDate 2022 33) 5000
        }
        `shouldBe`
        encode [aesonQQ|
          {
            "service_id": 5,
            "sort_order": 1,
            "area": "ARRAN",
            "route": "Ardrossan (ARD) - Brodick (BRO)",
            "status": 1,
            "additional_info": "Additional info",
            "disruption_reason": "Weather",
            "last_updated_date": "2022-02-01T01:23:20Z",
            "updated": "2022-02-02T01:23:20Z",
            "locations": [
              {
                "id": 4,
                "name": "Brodick",
                "latitude": 55.576606,
                "longitude": -5.139172,
                "weather": {
                  "temperature_celsius": 5,
                  "icon": "10n",
                  "wind_direction_cardinal": "SSW",
                  "wind_direction": 196.4,
                  "wind_speed_mph": 18,
                  "description": "Moderate rain"
                }
              }
            ]
          }
        |]

    describe "VesselResponse" $
        it "encodes correctly" $
          encode VesselResponse
          { vesselResponseMmsi = 232001580
          , vesselResponseName = "Caledonian Isles"
          , vesselResponseSpeed = Just 0.2
          , vesselResponseCourse = Just 226
          , vesselResponseLatitude = -5.135736
          , vesselResponseLongitude = 55.57808
          , vesselResponseLastReceived = UTCTime (fromOrdinalDate 2022 45) 6000
          }
          `shouldBe`
          encode [aesonQQ|
            {
              "mmsi": 232001580,
              "name": "Caledonian Isles",
              "speed": 0.2,
              "course": 226,
              "latitude": -5.135736,
              "longitude": 55.57808,
              "last_received": "2022-02-14T01:40:00Z"
            }
          |]

    describe "CreateInstallationRequest" $
      it "encodes correctly" $
        encode CreateInstallationRequest
        { createInstallationRequestDeviceToken = "20f2d2e336a0c62c8c5f3dc1bb3d3f36e830ce14230bc8fa547fd16fb8f917b5"
        , createInstallationRequestDeviceType = IOS
        }
        `shouldBe`
        encode [aesonQQ|
          {
            "device_token": "20f2d2e336a0c62c8c5f3dc1bb3d3f36e830ce14230bc8fa547fd16fb8f917b5",
            "device_type": "IOS"
          }
        |]

    describe "AddServiceRequest" $
      it "encodes correctly" $
        encode AddServiceRequest
        { addServiceRequestServiceID = 5
        }
        `shouldBe`
        encode [aesonQQ|
          {
            "service_id": 5
          }
        |]

    describe "DeviceType" $ do
      it "IOS encodes correctly" $
        encode IOS `shouldBe` encode [aesonQQ| "IOS" |]

      it "Android encodes correctly" $
        encode Android `shouldBe` encode [aesonQQ| "Android" |]

    describe "ServiceStatus" $ do
      it "Normal encodes correctly" $
        encode Normal `shouldBe` encode [aesonQQ| 0 |]

      it "Disrupted encodes correctly" $
        encode Disrupted `shouldBe` encode [aesonQQ| 1 |]

      it "Cancelled encodes correctly" $
        encode Cancelled `shouldBe` encode [aesonQQ| 2 |]

      it "Unknown encodes correctly" $
        encode Unknown `shouldBe` encode [aesonQQ| -99 |]
