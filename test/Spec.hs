{-# LANGUAGE OverloadedStrings #-}

import qualified IntegrationSpec
import qualified JSONSpec
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Integration Tests" IntegrationSpec.spec
  describe "JSON Tests" JSONSpec.spec
