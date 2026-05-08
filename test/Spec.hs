{-# LANGUAGE OverloadedStrings #-}

import qualified IntegrationSpec
import qualified JSONSpec
import qualified PushSpec
import qualified TransxchangeApiSpec
import qualified UtilitySpec
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Integration Tests" IntegrationSpec.spec
  describe "Push Tests" PushSpec.spec
  describe "TransXChange API Tests" TransxchangeApiSpec.spec
  describe "Utility Tests" UtilitySpec.spec
  describe "JSON Tests" JSONSpec.spec
