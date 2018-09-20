module Main where

import           Database.Persist.Sqlite (runMigration)

import qualified Database
import           Model                   (migrateAll)
import           Server.Environment      (Environment (..))

main :: IO ()
main = do

  -- Running the DB Migration
  Database.withHandle (Database.config env) $ \dbHandle ->
    Database.runDatabase dbHandle $ runMigration migrateAll

  return ()

  where
    env :: Environment
    env = Dev
