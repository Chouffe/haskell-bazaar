{-# LANGUAGE OverloadedStrings   #-}

module Server.Handler
  ( health
  , search
  )
  where

import           Data.Semigroup         ((<>))
import qualified Data.Text              as T


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ask)

import qualified Logger
import qualified Server.Monad

health :: Server.Monad.AppM T.Text
health = pure "OK"

search :: Maybe T.Text -> Server.Monad.AppM T.Text
search mQuery = do
  (loggerHandle, _) <- ask

  case mQuery of
    Nothing    -> do
      liftIO $ Logger.info loggerHandle ("No search query provided" :: T.Text)
      pure "No search was performed, return all recent ones"
    Just query -> do
      liftIO $ Logger.info loggerHandle $ "Search query provided " <> query
      pure $ "Search Results: " <> query
