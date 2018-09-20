{-# LANGUAGE OverloadedStrings          #-}

module Main where

import Model (migrateAll)

import           Control.Monad.Logger     (runStdoutLoggingT)
import           Database.Persist.Sqlite  (runMigration, runSqlite)

main :: IO ()
main = do
  -- Running the DB Migration
  runStdoutLoggingT
    $ runSqlite "dev.sqlite"
    $ runMigration migrateAll

  return ()
