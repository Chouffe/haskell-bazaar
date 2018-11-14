{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Maybe           (fromMaybe)
import           Data.Semigroup       ((<>))
import qualified Data.Text            as T
import           System.Environment   (lookupEnv)


import           Database.Persist.Sql (runMigration)

import qualified Database
import qualified Environment
import qualified Mailchimp
import qualified Logger
import           Model                (migrateAll)
import qualified Server
import qualified Server.Config

main :: IO ()
main = do
  maybeEnvString <- lookupEnv "ENVIRONMENT"
  let env :: Environment.Environment
      env = read $ fromMaybe "Dev" maybeEnvString

  let loggerConfig :: Logger.Config
      loggerConfig = Logger.config env

  let databaseConfig :: Database.Config
      databaseConfig = Database.config env

  let serverConfig :: Server.Config.Config
      serverConfig = Server.Config.config env

  let mailchimpConfig :: Mailchimp.Config
      mailchimpConfig = Mailchimp.config

  -- print env

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

        Server.run serverConfig loggerHandle databaseHandle mailchimpHandle
