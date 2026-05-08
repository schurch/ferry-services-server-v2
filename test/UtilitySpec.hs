module UtilitySpec where

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime (LocalTime), TimeOfDay (TimeOfDay))
import Test.Hspec (Spec, describe, it, shouldBe)
import Utility (convertScottishLocalTimeToUTC)

spec :: Spec
spec =
  describe "convertScottishLocalTimeToUTC" $ do
    it "keeps winter ferry times on GMT" $
      convertScottishLocalTimeToUTC (localTime 2026 3 16 9 30)
        `shouldBe` readUtc "2026-03-16 09:30:00 UTC"

    it "converts summer ferry times from BST to UTC" $
      convertScottishLocalTimeToUTC (localTime 2026 5 8 10 20)
        `shouldBe` readUtc "2026-05-08 09:20:00 UTC"

    it "handles the March daylight saving boundary using UK local clock rules" $ do
      convertScottishLocalTimeToUTC (localTime 2026 3 29 1 30)
        `shouldBe` readUtc "2026-03-29 01:30:00 UTC"
      convertScottishLocalTimeToUTC (localTime 2026 3 29 2 30)
        `shouldBe` readUtc "2026-03-29 01:30:00 UTC"

localTime :: Integer -> Int -> Int -> Int -> Int -> LocalTime
localTime year month day hour minute =
  LocalTime (fromGregorian year month day) (TimeOfDay hour minute 0)

readUtc :: String -> UTCTime
readUtc = read
