{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database
  ( Config
  , config
  , ConnectionString

  , Handle
  , withHandle

  , runDatabase

  -- DB queries
  , allItems
  , fillDatabaseTestValues
  , allItemsWithTags
  , keywords
  , searchByTagName
  , searchByAuthor
  )
  where

import           Control.Arrow           ((***))
import           Control.Exception       (bracket)
import           Control.Monad           (forM)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Data.Maybe              (maybe)
import           Data.Pool               (Pool, destroyAllResources,
                                          withResource)
import           Data.Semigroup          ((<>))
import qualified Data.Text               as T
import           Data.Time               (getCurrentTime)
import           Database.Esqueleto      (InnerJoin (..), from, in_, limit, on,
                                          select, val, valList, where_, (==.),
                                          (^.), (||.), ilike)

import           Database.Persist        (Entity (..), insert)
import           Database.Persist.Sql    (SqlBackend, SqlPersistT, runSqlConn)
import           Database.Persist.Sqlite (createSqlitePool, withSqliteConn)

import           Environment             (Environment (..))
import           Model
import qualified Model.Item              as ModelItem
import           Server.API.Types


type ConnectionString = T.Text

-- TODO: add logging type to handle or config
data Config
  = Config
    { cConnectionString :: ConnectionString
    , cPoolConnections  :: Int
    , cEnvironnment     :: Environment
    }
  deriving (Eq, Show)

data Handle
  = Handle
    { hMaybePool :: Maybe (Pool SqlBackend)
    , hConfig    :: Config
    }

config :: Environment -> Config
config Test = Config ":memory:" 0 Test
config Dev  = Config "dev.sqlite3" 0 Dev
config Prod = Config "prod.sqlite3" 5 Prod

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle cfg = bracket
  (mkHandle cfg)
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

allItems :: (MonadIO m) => (SqlPersistT m) [Item]
allItems = do
    entities <- select . from $ \users -> do
      limit 100
      return users
    return (entityVal <$> entities)

searchByTagName
  :: (MonadIO m)
  => T.Text
  -> (SqlPersistT m) [PublicItem]
searchByTagName query = do
  itemEntities <- itemsByTagName query
  hydrateItems itemEntities

searchByAuthor
  :: (MonadIO m)
  => T.Text
  -> (SqlPersistT m) [PublicItem]
searchByAuthor query = do
  itemEntities <- itemsByAuthorName query
  hydrateItems itemEntities

itemsByAuthorName
  :: (MonadIO m)
  => T.Text    -- ^ Tag Name
  -> (SqlPersistT m) [Entity Item]
itemsByAuthorName query = do
  items <- select . from $ \(items' `InnerJoin` itemAuthors' `InnerJoin` authors') -> do
    on (itemAuthors' ^. ItemAuthorAuthorId ==. authors' ^. AuthorId)
    on (items' ^. ItemId ==. itemAuthors' ^. ItemAuthorItemId)
    where_
      ( authors' ^. AuthorFirstName ==. val query
        ||. authors' ^. AuthorLastName ==. val query
      )
      -- TODO: use ilike instead
      -- And concatenate firstname and lastname to perform the search
    limit 100
    return items'
  return items

itemsByTagName
  :: (MonadIO m)
  => T.Text    -- ^ Tag Name
  -> (SqlPersistT m) [Entity Item]
itemsByTagName query = do
  items <- select . from $ \(items' `InnerJoin` itemTags' `InnerJoin` tags') -> do
    on (itemTags' ^. ItemTagTagId ==. tags' ^. TagId)
    on (items' ^. ItemId ==. itemTags' ^. ItemTagItemId)
    where_ (tags' ^. TagName ==. val query)
    limit 100
    return items'
  return items

hydrateItems
  :: (MonadIO m)
  => [Entity Item]
  -> (SqlPersistT m) [PublicItem]
hydrateItems itemEntities = do

  tags :: [(Entity Item, Entity Tag)] <-
    select . from $ \(items' `InnerJoin` itemTags' `InnerJoin` tags') -> do
      on (itemTags' ^. ItemTagTagId ==. tags' ^. TagId)
      on (items' ^. ItemId ==. itemTags' ^. ItemTagItemId)
      where_ $ items' ^. ItemId `in_` valList itemIds
      return (items', tags')

  authors :: [(Entity Item, Entity Author)] <-
    select . from $ \(items' `InnerJoin` itemAuthors' `InnerJoin` authors') -> do
      on (itemAuthors' ^. ItemAuthorAuthorId ==. authors' ^. AuthorId)
      on (items' ^. ItemId ==. itemAuthors' ^. ItemAuthorItemId)
      where_ $ items' ^. ItemId `in_` valList itemIds
      return (items', authors')

  let itemTags    = toMap tags
  let itemAuthors = toMap authors

  return $ do
    itemEntity <- itemEntities
    return PublicItem
      { piItem    = entityVal itemEntity
      , piTags    = entityVal <$> fromMaybe [] (M.lookup itemEntity itemTags)
      , piAuthors = entityVal <$> fromMaybe [] (M.lookup itemEntity itemAuthors)
      }

    where
      itemIds :: [ItemId]
      itemIds = entityKey <$> itemEntities

      toMap :: [(Entity Item, a)] -> M.Map (Entity Item) [a]
      toMap xs = M.fromListWith (++) (fmap (\(i, t) -> (i, [t])) xs)

-- TODO: how to get items without tags? Using a LeftOuterJoin but I am getting type errors
allItemsWithTags :: (MonadIO m) => (SqlPersistT m) (M.Map Item [Tag])
allItemsWithTags = do

    tuples <- select . from $ \(items `InnerJoin` itemTags `InnerJoin` tags) -> do
      on (itemTags ^. ItemTagTagId ==. tags ^. TagId)
      on (items ^. ItemId ==. itemTags ^. ItemTagItemId)
      limit 100
      return (items, tags)
    return $ toMap $ (entityVal *** entityVal) <$> tuples

    where
      toMap :: [(Item, Tag)] -> M.Map Item [Tag]
      toMap xs = M.fromListWith (++) (fmap (\(i, t) -> (i, [t])) xs)

allTags :: MonadIO m => (SqlPersistT m) [Tag]
allTags = do
  results <- select . from $ return
  return (entityVal <$> results)

allAuthors :: MonadIO m => (SqlPersistT m) [Author]
allAuthors = do
  results <- select . from $ return
  return (entityVal <$> results)

keywords :: MonadIO m => (SqlPersistT m) [PublicKeyword]
keywords = do
  tags    <- allTags
  authors <- allAuthors
  return $ (PublicTag <$> tags) <> (PublicAuthor <$> authors)

fillDatabaseTestValues :: (MonadIO m) => SqlPersistT m ()
fillDatabaseTestValues = do

  -- Getting the current time
  t <- liftIO getCurrentTime

  -- Tags
  tagKeys <- forM ["haskell", "monad", "categories", "functor"] (insert . Tag)

  -- Authors
  authorKeys <- forM [("Simon", "Marlow"), ("Rich", "Hickey")] $ \(firstName, lastName) ->
    insert (Author firstName lastName)

  -- Items
  itemKeys <- forM
    [ ("title1", "description1", "url1", ModelItem.Article)
    , ("title2", "description2", "url2", ModelItem.Video)
    , ("title3", "description3", "url3", ModelItem.Article)
    ] $ \(title, description, url, itemType) ->
      insert $ Item title (Just description) url itemType t

  -- Relations: Tag <-> Item
  _ <- insert $ ItemTag (itemKeys !! 0) (tagKeys !! 0)
  _ <- insert $ ItemTag (itemKeys !! 0) (tagKeys !! 1)
  _ <- insert $ ItemTag (itemKeys !! 0) (tagKeys !! 2)
  _ <- insert $ ItemTag (itemKeys !! 1) (tagKeys !! 0)
  _ <- insert $ ItemTag (itemKeys !! 2) (tagKeys !! 2)

  -- Relations: Author <-> Item
  _ <- insert $ ItemAuthor (itemKeys !! 0) (authorKeys !! 0)
  _ <- insert $ ItemAuthor (itemKeys !! 0) (authorKeys !! 1)
  _ <- insert $ ItemAuthor (itemKeys !! 1) (authorKeys !! 0)
  _ <- insert $ ItemAuthor (itemKeys !! 2) (authorKeys !! 1)

  return ()
