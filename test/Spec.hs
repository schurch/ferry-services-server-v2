{-# LANGUAGE OverloadedStrings #-}

import qualified IntegrationSpec
import qualified JSONSpec
import qualified TransxchangeApiSpec
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Integration Tests" IntegrationSpec.spec
  describe "TransXChange API Tests" TransxchangeApiSpec.spec
  describe "JSON Tests" JSONSpec.spec
