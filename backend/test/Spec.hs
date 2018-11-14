{-# LANGUAGE OverloadedStrings #-}

import           Data.Semigroup          ((<>))
import qualified Data.Text               as T

import           Database.Persist.Sql    (runMigration)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (ClientEnv (..), parseBaseUrl)
import           Test.Tasty.Hspec        (hspec)

import qualified Database
import qualified Environment
import qualified Logger
import qualified Mailchimp
import           Model                   (migrateAll)
import qualified Server
import qualified Server.API
import qualified Server.APISpec
import qualified Server.Config


main :: IO ()
main =
  -- Integration Tests
  withApp $ \cfg -> do
    clientEnv <- mkClientEnv cfg
    hspec $ Server.APISpec.spec clientEnv
  -- TODO: Add Unit tests

mkClientEnv
  :: Server.Config.Config
  -> IO ClientEnv
mkClientEnv config = do
  mgr        <- newManager tlsManagerSettings
  baseUrl    <- parseBaseUrl
    $ "http://localhost:" <> show (Server.Config.cPort config)
  return $ ClientEnv mgr baseUrl

withApp :: (Server.Config.Config -> IO a) -> IO a
withApp f =
  Logger.withHandle loggerConfig $ \loggerHandle -> do

    Logger.info loggerHandle ("Starting Database" :: T.Text)
    Logger.info loggerHandle (show databaseConfig)

    Database.withHandle databaseConfig $ \databaseHandle -> do

      Logger.info loggerHandle ("Running Database Migration" :: T.Text)
      Database.runDatabase databaseHandle (runMigration migrateAll)

      Logger.info loggerHandle
        ("Starting Server on port " <> show (Server.Config.cPort serverConfig))
      Logger.info loggerHandle (show serverConfig)

      Mailchimp.withHandle loggerHandle mailchimpConfig $ \mailchimpHandle ->
        Server.withServer serverConfig loggerHandle databaseHandle mailchimpHandle (f serverConfig)

  where
    env :: Environment.Environment
    env = Environment.Test

    loggerConfig :: Logger.Config
    loggerConfig = Logger.config env

    databaseConfig :: Database.Config
    databaseConfig = Database.config env

    serverConfig :: Server.Config.Config
    serverConfig = Server.Config.config env

    mailchimpConfig :: Mailchimp.Config
    mailchimpConfig = Mailchimp.config
