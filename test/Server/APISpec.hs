{-# LANGUAGE OverloadedStrings #-}

module Server.APISpec (spec) where

import           Data.Semigroup          ((<>))

import           Servant.Client          (ClientEnv, parseBaseUrl, runClientM)
import           Test.Tasty.Hspec        (Spec, shouldBe, it, describe)

import qualified Server.API
import qualified Server.Client

spec :: ClientEnv -> Spec
spec clientEnv =
  describe "/health" $
    it "is healthy" $ do
      result <- runClientM Server.Client.health clientEnv
      result `shouldBe` Right "OK"
