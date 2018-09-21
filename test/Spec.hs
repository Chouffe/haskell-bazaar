{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import           Data.Semigroup          ((<>))

import           Database.Persist.Sqlite (runMigration)
-- import           Test.Tasty              (defaultMain, testGroup)
-- import           Test.Tasty.Hspec        (testSpec, hspec)
import           Test.Tasty.Hspec        (hspec)
import           Servant.Client          (ClientEnv (..), parseBaseUrl)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Server.APISpec
import qualified Server.API
import qualified Database
import qualified Logger
import qualified Environment
import           Model                   (migrateAll)


main :: IO ()
main =

  -- Integration Tests
  withApp $ \handle -> do
    clientEnv <- mkClientEnv $ Server.API.hConfig handle
    hspec $ Server.APISpec.spec clientEnv

  -- TODO: Add Unit tests

mkClientEnv
  :: Server.API.Config
  -> IO ClientEnv
mkClientEnv config = do
  mgr        <- newManager tlsManagerSettings
  baseUrl    <- parseBaseUrl $ "http://localhost:" <> show (Server.API.cPort config)
  return $ ClientEnv mgr baseUrl

withApp
  :: (Server.API.Handle -> IO a)
  -> IO a
withApp f =
  Logger.withHandle loggerConfig $ \loggerHandle -> do

    Logger.info loggerHandle ("Starting Database" :: T.Text)
    Logger.info loggerHandle (show databaseConfig)

    Database.withHandle databaseConfig $ \databaseHandle -> do

      Logger.info loggerHandle ("Running Database Migration" :: T.Text)
      Database.runDatabase databaseHandle (runMigration migrateAll)

      Logger.info loggerHandle ("Starting Server on port " <> show (Server.API.cPort serverConfig))
      Logger.info loggerHandle (show serverConfig)

      Server.API.withServer serverConfig loggerHandle databaseHandle $ \serverHandle ->
        f serverHandle

  where
    env :: Environment.Environment
    env = Environment.Test

    loggerConfig :: Logger.Config
    loggerConfig = Logger.config env

    databaseConfig :: Database.Config
    databaseConfig = Database.config env

    serverConfig :: Server.API.Config
    serverConfig = Server.API.config env
