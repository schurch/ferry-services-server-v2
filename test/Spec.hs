{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec      (Spec, describe, hspec)

import qualified IntegrationSpec
import qualified JSONTests

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Integration Tests" IntegrationSpec.spec
  describe "JSON Tests" JSONTests.spec
