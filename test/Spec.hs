{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec      (Spec, describe, hspec)

import qualified IntegrationSpec
import qualified JSONSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Integration Tests" IntegrationSpec.spec
  describe "JSON Tests" JSONSpec.spec
