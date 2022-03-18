{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module JSONTests where

import           Data.Aeson    (encode)
import           Data.Aeson.QQ (aesonQQ)
import           Test.Hspec    (Spec, describe, it, shouldBe)

import           Types

spec :: Spec
spec = do
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

