{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Server.Handler
  ( health
  , keywords
  , search
  , root
  )
  where

import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (asks)
import           Control.Monad.Reader.Class       (MonadReader)
import qualified Data.ByteString.Lazy             as BS
import           Data.Semigroup                   ((<>))
import qualified Data.Text                        as T

import           Servant.API.ContentTypesExtended (RawHtml (..))

import qualified Logger
import qualified Database
import           Server.API.Types
import qualified Server.Config


health :: Monad m => m T.Text
health = pure "OK"

search
  :: (MonadReader Server.Config.Handle m, MonadIO m)
  => Maybe T.Text  -- ^ Optional Search query
  -> m [PublicItem]
search mQuery = do
  loggerHandle   <- asks Server.Config.hLogger
  databaseHandle <- asks Server.Config.hDB

  case mQuery of
    Nothing    -> do
      liftIO $ Logger.info loggerHandle ("No search query provided" :: T.Text)
      pure []

    Just query -> do
      liftIO $ Logger.info loggerHandle $ "Search query provided " <> query
      itemsByTags <- liftIO $ Database.runDatabase databaseHandle (Database.searchByTagName query)
      itemsByAuthors <- liftIO $ Database.runDatabase databaseHandle (Database.searchByAuthor query)
      pure $ itemsByTags <> itemsByAuthors

root :: MonadIO m => m RawHtml
root = do
  content <- liftIO $ BS.readFile "static/index.html"
  return $ RawHtml content

keywords
  :: (MonadReader Server.Config.Handle m, MonadIO m)
  => m [PublicKeyword]
keywords = do
  databaseHandle <- asks Server.Config.hDB
  liftIO $ Database.runDatabase databaseHandle Database.keywords
