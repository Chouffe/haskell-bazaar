{-# LANGUAGE OverloadedStrings #-}

module Server.APISpec (spec) where

import           Data.Semigroup         ((<>))

import           Control.Lens           ((^.))
import qualified Data.ByteString.Lazy   as BS
import qualified Network.Wreq           as W
import           Servant.Client         (ClientEnv (..), parseBaseUrl,
                                         runClientM)
import           Servant.Common.BaseUrl (showBaseUrl)
import           Test.Tasty.Hspec       (Spec, describe, it, shouldBe)

import qualified Server.API
import qualified Server.Client


spec :: ClientEnv -> Spec
spec clientEnv = do

  describe "/health" $
    it "is healthy" $ do
      result <- runClientM Server.Client.health clientEnv
      result `shouldBe` Right "OK"

  describe "/" $
    it "serves static/index.html" $ do
      indexFile <- BS.readFile indexFilePath
      result    <- W.get $ showBaseUrl baseUrl
      result ^. W.responseBody `shouldBe` indexFile

  describe "/index.html" $
    it "serves static/index.html" $ do
      result    <- W.get $ showBaseUrl baseUrl <> "/index.html"
      indexFile <- BS.readFile indexFilePath
      result ^. W.responseBody `shouldBe` indexFile

  where
    ClientEnv _ baseUrl = clientEnv

    indexFilePath :: FilePath
    indexFilePath = "static/index.html"
