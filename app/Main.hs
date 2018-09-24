{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Semigroup          ((<>))
import qualified Data.Text               as T

import           Database.Persist.Sqlite (runMigration)

import qualified Database
import qualified Environment
import qualified Logger
import           Model                   (migrateAll)
import qualified Server
import qualified Server.Config

main :: IO ()
main =
  Logger.withHandle loggerConfig $ \loggerHandle -> do

    Logger.info loggerHandle ("Starting Database" :: T.Text)
    Logger.info loggerHandle (show databaseConfig)

    Database.withHandle databaseConfig $ \databaseHandle -> do

      Logger.info loggerHandle ("Running Database Migration" :: T.Text)
      Database.runDatabase databaseHandle (runMigration migrateAll)

      Logger.info loggerHandle
        ("Starting Server on port " <> show (Server.Config.cPort serverConfig))
      Logger.info loggerHandle (show serverConfig)

      Server.run serverConfig loggerHandle databaseHandle

  where
    -- TODO: read CL arguments
    env :: Environment.Environment
    env = Environment.Dev

    loggerConfig :: Logger.Config
    loggerConfig = Logger.config env

    databaseConfig :: Database.Config
    databaseConfig = Database.config env

    serverConfig :: Server.Config.Config
    serverConfig = Server.Config.config env
