{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database
  ( Config
  , config
  , ConnectionString

  , Handle
  , withHandle

  , runDatabase

  -- DB queries
  , allItems
  , insertItems
  , fillDatabaseTestValues
  , allItemsWithTags
  )
  where

import Control.Arrow ((***))
import           Control.Exception       (bracket)
import           Control.Monad           (forM, forM_)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Logger    (LoggingT, MonadLogger,
                                          runStdoutLoggingT)
import qualified Data.Map                as M
import           Data.Maybe              (maybe)
import           Data.Pool               (Pool, destroyAllResources,
                                          withResource)
import qualified Data.Text               as T
import           Data.Time               (UTCTime, getCurrentTime)
import           Database.Esqueleto      (InnerJoin (..), LeftOuterJoin (..),
                                          desc, from, limit, on, orderBy,
                                          select, val, where_, (==.), (^.))

import           Database.Persist        (Entity (..), insert)
import           Database.Persist.Sql    (SqlBackend, SqlPersistT, fromSqlKey,
                                          runSqlConn, toSqlKey)
import           Database.Persist.Sqlite (createSqlitePool, runSqlite,
                                          withSqliteConn, withSqlitePool)

import           Model
import qualified Model.Item              as ModelItem
import           Server.Environment      (Environment (..))


type ConnectionString = T.Text

-- TODO: add logging type to handle or config
data Config
  = Config
    { cConnectionString :: ConnectionString
    , cPoolConnections  :: Int
    }
  deriving (Eq, Show)

data Handle
  = Handle
    { hMaybePool :: Maybe (Pool SqlBackend)
    , hConfig    :: Config
    }

config :: Environment -> Config
config Test = Config ":memory:" 0
config Dev  = Config "dev.sqlite3" 0
config Prod = Config "prod.sqlite3" 5

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle cfg =
  bracket (mkHandle cfg)
          (\Handle {..} -> maybe (return ()) destroyAllResources hMaybePool)

mkHandle :: Config -> IO Handle
mkHandle cfg@Config {..}
  | cPoolConnections > 0 = do
      pool <- runStdoutLoggingT $ createSqlitePool cConnectionString cPoolConnections
      return $ Handle (Just pool) cfg
  | otherwise = return $ Handle Nothing cfg

runDatabase :: Handle -> SqlPersistT (LoggingT IO) a -> IO a
runDatabase Handle {..} query =
  runStdoutLoggingT $
    case hMaybePool of
      Nothing   -> withSqliteConn (cConnectionString hConfig) (runSqlConn query)
      Just pool -> withResource pool (runSqlConn query)

allItems :: (MonadIO m, MonadLogger m) => (SqlPersistT m) [Item]
allItems = do
    entities <- select . from $ \users -> do
      limit 100
      return users
    return (entityVal <$> entities)

insertItems :: (MonadIO m, MonadLogger m) => (SqlPersistT m) ()
insertItems = do
  t <- liftIO getCurrentTime
  k <- insert (mkItem1 t)
  liftIO $ print k
  return ()

-- TODO: how to get items without tags?
allItemsWithTags :: (MonadIO m, MonadLogger m) => (SqlPersistT m) (M.Map Item [Tag])
allItemsWithTags = do

    tuples <- select . from $ \(items `InnerJoin` itemTags `InnerJoin` tags) -> do
      on (items ^. ItemId ==. itemTags ^. ItemTagItemId)
      on (itemTags ^. ItemTagTagId ==. tags ^. TagId)
      limit 100
      return (items, tags)
    return $ toMap $ (entityVal *** entityVal) <$> tuples

    where
      toMap :: [(Item, Tag)] -> M.Map Item [Tag]
      toMap xs = M.fromListWith (++) (fmap (\(i, t) -> (i, [t])) xs)


mkItem1 :: UTCTime -> Item
mkItem1 = Item "title" (Just "description") "url" ModelItem.Article

fillDatabaseTestValues :: (MonadIO m, MonadLogger m) => SqlPersistT m ()
fillDatabaseTestValues = do

  -- Getting the current time
  t <- liftIO getCurrentTime

  -- Tags
  tagKeys <- forM ["haskell", "monad", "categories", "functor"] (insert . Tag)

  -- Authors
  authorKeys <- forM [("Simon", "Marlow"), ("Rich", "Hickey")] $ \(first, last) ->
    insert (Author first last)

  -- Items
  itemKeys <- forM
    [ ("title1", "description1", "url1", ModelItem.Article)
    , ("title2", "description2", "url2", ModelItem.Video)
    , ("title3", "description3", "url3", ModelItem.Article)
    ] $ \(title, description, url, itemType) ->
      insert $ Item title (Just description) url itemType t

  -- Relations: Tag <-> Item
  insert $ ItemTag (itemKeys !! 0) (tagKeys !! 0)
  insert $ ItemTag (itemKeys !! 0) (tagKeys !! 1)
  insert $ ItemTag (itemKeys !! 0) (tagKeys !! 2)
  insert $ ItemTag (itemKeys !! 1) (tagKeys !! 0)
  insert $ ItemTag (itemKeys !! 2) (tagKeys !! 2)

  -- Relations: Author <-> Item
  insert $ ItemAuthor (itemKeys !! 0) (authorKeys !! 0)
  insert $ ItemAuthor (itemKeys !! 0) (authorKeys !! 1)
  insert $ ItemAuthor (itemKeys !! 1) (authorKeys !! 0)
  insert $ ItemAuthor (itemKeys !! 2) (authorKeys !! 1)

  return ()
